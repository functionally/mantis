
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Transaction (
  makeTransaction
, includeFee
, supportedMultiAsset
, readMetadata
, printUTxO
, printValue
, summarizeValues
, makeMinting
, readMinting
) where


import Cardano.Api (AssetId(..), AssetName(..), AuxScriptsSupportedInEra(..), CardanoEra(..), MultiAssetSupportedInEra(..), Hash, MaryEra, NetworkId, PaymentKey, PolicyId(..), Quantity(..), ScriptInEra, SlotNo(..), TxAuxScripts(..), TxCertificates(..), TxFee(..), TxFeesExplicitInEra(..), TxMetadata, TxMetadataInEra(..), TxMetadataSupportedInEra(..), TxMetadataJsonSchema(..), TxMintValue(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), ValidityNoUpperBoundSupportedInEra(..), ValidityUpperBoundSupportedInEra(..), Value, auxScriptsSupportedInEra, estimateTransactionFee, filterValue, lovelaceToValue, makeSignedTransaction, makeTransactionBody, metadataFromJson, multiAssetSupportedInEra, negateValue, selectLovelace, serialiseToRawBytesHex, txFeesExplicitInEra, validityNoUpperBoundSupportedInEra, validityUpperBoundSupportedInEra, valueFromList, valueToList)
import Cardano.Api.Shelley (ProtocolParameters, TxBodyContent(..), TxId(..), TxIn(..), TxOut(..), TxOutValue(..), UTxO(..), protocolParamTxFeeFixed, protocolParamTxFeePerByte)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM, foistMantisEither, foistMantisMaybeIO)

import qualified Data.Aeson            as A  (Value(..), decodeFileStrict)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.HashMap.Strict   as H  (keys, singleton)
import qualified Data.Map.Strict       as M  (assocs)
import qualified Data.Text             as T  (pack, unpack)


supportedNoUpperBound :: ValidityNoUpperBoundSupportedInEra MaryEra
Just supportedNoUpperBound = validityNoUpperBoundSupportedInEra MaryEra


supportedUpperBound :: ValidityUpperBoundSupportedInEra MaryEra
Just supportedUpperBound = validityUpperBoundSupportedInEra MaryEra


supportedMultiAsset :: MultiAssetSupportedInEra MaryEra
Right supportedMultiAsset = multiAssetSupportedInEra MaryEra


supportedScripts :: AuxScriptsSupportedInEra MaryEra
Just supportedScripts = auxScriptsSupportedInEra MaryEra


explicitFees :: TxFeesExplicitInEra MaryEra
Right explicitFees = txFeesExplicitInEra MaryEra


makeTransaction :: [TxIn]
                -> [TxOut MaryEra]
                -> Maybe SlotNo
                -> Maybe TxMetadata
                -> Maybe (ScriptInEra MaryEra)
                -> Maybe Value
                -> TxBodyContent MaryEra
makeTransaction txIns txOuts before metadata script minting =
  let
    txFee            = TxFeeExplicit explicitFees 0
    txValidityRange  = (
                         TxValidityNoLowerBound
                       , maybe
                           (TxValidityNoUpperBound supportedNoUpperBound)
                           (TxValidityUpperBound supportedUpperBound)
                           before
                       )
    txMetadata       = maybe TxMetadataNone (TxMetadataInEra TxMetadataInMaryEra) metadata
    txAuxScripts     = maybe TxAuxScriptsNone (TxAuxScripts supportedScripts . pure) script
    txWithdrawals    = TxWithdrawalsNone
    txCertificates   = TxCertificatesNone
    txUpdateProposal = TxUpdateProposalNone
    txMintValue      = maybe TxMintNone (TxMintValue supportedMultiAsset) minting
  in
    TxBodyContent{..}


includeFee :: MonadFail m
           => MonadIO m
           => NetworkId
           -> ProtocolParameters
           -> Int
           -> Int
           -> Int
           -> Int
           -> TxBodyContent MaryEra
           -> MantisM m (TxBodyContent MaryEra)
includeFee network pparams nIn nOut nShelley nByron content =
  do
    body <- foistMantisEither $ makeTransactionBody content
    let
      tx = makeSignedTransaction [] body
      lovelace = estimateTransactionFee
        network
        (protocolParamTxFeeFixed   pparams)
        (protocolParamTxFeePerByte pparams)
        tx
        nIn nOut nShelley nByron
    [TxOut addr (TxOutValue s value)] <- return $ txOuts content
    let
      fee = negateValue $ lovelaceToValue lovelace
    return
      $ content
        {
          txFee  = TxFeeExplicit explicitFees lovelace
        , txOuts = [TxOut addr (TxOutValue s $ value <> fee)]
        }


readMetadata' :: MonadIO m
              => FilePath
              -> MantisM m (A.Value, TxMetadata)
readMetadata' filename =
  do
    json <-
      foistMantisMaybeIO "Cound not decode metadata."
        $ A.decodeFileStrict filename
    metadata <-
      foistMantisEither
        $ metadataFromJson TxMetadataJsonNoSchema json
    return (json, metadata)


readMetadata :: MonadIO m
             => FilePath
             -> MantisM m TxMetadata
readMetadata = fmap snd . readMetadata'


printUTxO :: MonadIO m
          => String
          -> UTxO MaryEra
          -> MantisM m ()
printUTxO indent (UTxO utxoMap) =
  sequence_
    [
      do
        liftIO
          . putStrLn
          $ indent ++ "Transaction: " ++ show' txhash  ++ "#" ++ show txin
        printValue (indent ++ "  ") value'
    |
      (TxIn (TxId txhash) txin, TxOut _ (TxOutValue _ value')) <- M.assocs utxoMap
    ]


printValue :: MonadIO m
           => String
           -> Value
           -> MantisM m ()
printValue indent value =
  liftIO
    $ do
      putStrLn $ indent ++ show (selectLovelace value) ++ " Lovelace"
      sequence_
        [
          putStrLn $ indent ++ show quantity ++ "  " ++ show' policy ++ "." ++ show' asset
        |
          (AssetId (PolicyId policy) (AssetName asset), quantity) <- valueToList $ filterValue (/= AdaAssetId) value
        ]


show' :: Show a => a -> String
show' = init . tail . show


summarizeValues :: UTxO MaryEra
                -> (Int, Value)
summarizeValues (UTxO utxoMap) =
  let
    values =  
      [
        value'
      |
        (_, TxOut _ (TxOutValue _ value')) <- M.assocs utxoMap
      ]
  in
    (length values, mconcat values)



makeMinting :: Value
            -> String
            -> Integer
            -> Hash PaymentKey
            -> Maybe SlotNo
            -> (ScriptInEra MaryEra, Value, Value)
makeMinting value name count verification before =
  let
    (script, scriptHash) = mintingScript verification before
    minting = valueFromList
      [(
        AssetId (PolicyId scriptHash) (AssetName $ BS.pack name)
      , Quantity count
      )]
  in
    (
      script
    , minting
    , value <> minting
    )


readMinting :: MonadFail m
            => MonadIO m
            => PolicyId
            -> FilePath
            -> MantisM m (A.Value, TxMetadata, Value)
readMinting policyId filename =
  do
    (A.Object json, metadata) <- readMetadata' filename
    let
      minting =
        valueFromList
          [
            (
              AssetId policyId . AssetName . BS.pack $ T.unpack name
            , Quantity 1
            )
          |
            name <- H.keys json
          ]
      json' =
          A.Object
          . H.singleton "721"
          . A.Object
          $ H.singleton
            (T.pack . BS.unpack $ serialiseToRawBytesHex policyId)
            (A.Object json)
    return (json', metadata, minting)
