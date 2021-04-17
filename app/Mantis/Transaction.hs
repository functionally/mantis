
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Transaction (
  makeTransaction
, includeFee
, fromShelleyUTxO
, supportedMultiAsset
, readMetadata
, printUTxO
, printValue
, summarizeValues
, makeMinting
, readMinting
) where


import Cardano.Api.Shelley (TxBodyContent(..), TxId(..), TxIn(..), TxOut(..), TxOutValue(..), fromMaryValue, fromShelleyAddr)
import Cardano.Api.Eras (CardanoEra(..), MaryEra, ShelleyLedgerEra)
import Cardano.Api.Typed (AssetId(..), AssetName(..), AuxScriptsSupportedInEra(..), MultiAssetSupportedInEra(..), Hash, NetworkId, PaymentKey, PolicyId(..), Quantity(..), ScriptInEra, SlotNo(..), TxAuxScripts(..), TxCertificates(..), TxFee(..), TxFeesExplicitInEra(..), TxMetadata, TxMetadataInEra(..), TxMetadataSupportedInEra(..), TxMetadataJsonSchema(..), TxMintValue(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), ValidityNoUpperBoundSupportedInEra(..), ValidityUpperBoundSupportedInEra(..), Value, auxScriptsSupportedInEra, estimateTransactionFee, lovelaceToValue, makeSignedTransaction, makeTransactionBody, metadataFromJson, multiAssetSupportedInEra, negateValue, serialiseToRawBytesHex, txFeesExplicitInEra, validityNoUpperBoundSupportedInEra, validityUpperBoundSupportedInEra, valueFromList)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM, foistMantisEither, foistMantisMaybeIO)

import qualified Cardano.Ledger.Mary.Value            as Mary      (AssetName(..), PolicyID(..), Value(..))
import qualified Data.Aeson                           as A         (Value(..), decodeFileStrict)
import qualified Data.ByteString.Char8                as BS        (pack, unpack)
import qualified Data.HashMap.Strict                  as H         (keys, singleton)
import qualified Data.Map.Strict                      as M         (Map, assocs, fromList)
import qualified Data.Text                            as T         (pack, unpack)
import qualified Ouroboros.Consensus.Shelley.Eras     as Ouroboros (StandardMary)
import qualified Shelley.Spec.Ledger.PParams          as Shelley   (PParams)
import qualified Shelley.Spec.Ledger.API              as Shelley   (PParams'(..), ScriptHash(..))
import qualified Shelley.Spec.Ledger.TxBody           as Shelley   (TxId(..), TxIn(..), TxOut(..))
import qualified Shelley.Spec.Ledger.UTxO             as Shelley   (UTxO(..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Ouroboros (StandardCrypto)


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
           -> Shelley.PParams (ShelleyLedgerEra MaryEra)
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
        (Shelley._minfeeB pparams)
        (Shelley._minfeeA pparams)
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


fromShelleyUTxO :: Shelley.UTxO Ouroboros.StandardMary
                -> M.Map TxIn (TxOut MaryEra)
fromShelleyUTxO (Shelley.UTxO utxoMap) =
  M.fromList
    [
      (
        TxIn (TxId txhash) (toEnum . fromEnum $ txin)
      , TxOut (fromShelleyAddr addr) (TxOutValue supportedMultiAsset $ fromMaryValue value)
      )
    |
      (Shelley.TxIn (Shelley.TxId txhash) txin, Shelley.TxOut addr value) <- M.assocs utxoMap
    ] 


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
          -> Shelley.UTxO Ouroboros.StandardMary
          -> MantisM m ()
printUTxO indent (Shelley.UTxO utxoMap) =
  sequence_
    [
      do
        liftIO
          . putStrLn
          $ indent ++ "Transaction: " ++ show' txhash  ++ "#" ++ show txin
        printValue (indent ++ "  ") value'
    |
      (Shelley.TxIn (Shelley.TxId txhash) txin, Shelley.TxOut _ value') <- M.assocs utxoMap
    ]


printValue :: MonadIO m
           => String
           -> Mary.Value era
           -> MantisM m ()
printValue indent (Mary.Value lovelace policies) =
  liftIO
    $ do
      putStrLn $ indent ++ show lovelace ++ " Lovelace"
      sequence_
        [
          putStrLn $ indent ++ show quantity ++ "  " ++ show' policy ++ "." ++ show' asset
        |
          (Mary.PolicyID (Shelley.ScriptHash policy), assets) <- M.assocs policies
        , (Mary.AssetName asset, quantity) <- M.assocs assets
        ]


show' :: Show a => a -> String
show' = init . tail . show


summarizeValues :: Shelley.UTxO Ouroboros.StandardMary
                -> (Int, Mary.Value Ouroboros.StandardCrypto)
summarizeValues (Shelley.UTxO utxoMap) =
  let
    values =  
      [
        value'
      |
        (_, Shelley.TxOut _ value') <- M.assocs utxoMap
      ]
  in
    (length values, mconcat values)



makeMinting :: Mary.Value Ouroboros.StandardCrypto
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
    , fromMaryValue value <> minting
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
