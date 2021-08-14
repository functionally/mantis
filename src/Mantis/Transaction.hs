-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Handling transactions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Mantis.Transaction (
-- * Transactions
  makeTransaction
, includeFee
-- * Metadata
, readMetadata
-- * Printing
, printUTxO
, printValue
, printValueIO
, summarizeValues
-- * Minting
, makeMinting
, readMinting
, supportedMultiAsset
) where


import Cardano.Api (AssetId(..), AssetName(..), AuxScriptsSupportedInEra(..), BuildTx, BuildTxWith(..), CardanoEra(..), MultiAssetSupportedInEra(..), Hash, KeyWitnessInCtx(..), MaryEra, NetworkId, PaymentKey, PolicyId(..), Quantity(..), ScriptLanguageInEra(..), SimpleScript(..), SimpleScriptV2, SimpleScriptVersion(..), ScriptWitness(..), SlotNo(..), TxAuxScripts(..), TxCertificates(..), TxExtraScriptData(..), TxExtraKeyWitnesses(..), TxFee(..), TxFeesExplicitInEra(..), TxInsCollateral(..), TxMetadata, TxMetadataInEra(..), TxMetadataSupportedInEra(..), TxMetadataJsonSchema(..), TxMintValue(..), TxOutDatumHash(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), ValidityNoUpperBoundSupportedInEra(..), ValidityUpperBoundSupportedInEra(..), Value, Witness(..), auxScriptsSupportedInEra, estimateTransactionFee, filterValue, lovelaceToValue, makeSignedTransaction, makeTransactionBody, metadataFromJson, multiAssetSupportedInEra, negateValue, selectLovelace, serialiseToRawBytesHex, txFeesExplicitInEra, validityNoUpperBoundSupportedInEra, validityUpperBoundSupportedInEra, valueFromList, valueToList)
import Cardano.Api.Shelley (ProtocolParameters, TxBodyContent(..), TxId(..), TxIn(..), TxOut(..), TxOutValue(..), UTxO(..), protocolParamTxFeeFixed, protocolParamTxFeePerByte)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM, foistMantisEither, foistMantisMaybeIO)

import qualified Data.Aeson            as A  (Value(..), decodeFileStrict)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.HashMap.Strict   as H  (keys, singleton)
import qualified Data.Map.Strict       as M  (assocs, singleton)
import qualified Data.Text             as T  (pack, unpack)


-- | The Mary era supports having no upper bound.
supportedNoUpperBound :: ValidityNoUpperBoundSupportedInEra MaryEra
Just supportedNoUpperBound = validityNoUpperBoundSupportedInEra MaryEra


-- | The Mary era support upper bounds.
supportedUpperBound :: ValidityUpperBoundSupportedInEra MaryEra
Just supportedUpperBound = validityUpperBoundSupportedInEra MaryEra


-- | The Mary era supports multi-assets.
supportedMultiAsset :: MultiAssetSupportedInEra MaryEra
Right supportedMultiAsset = multiAssetSupportedInEra MaryEra


-- | The Mary era has explicit fees.
explicitFees :: TxFeesExplicitInEra MaryEra
Right explicitFees = txFeesExplicitInEra MaryEra


-- | Build a transaction.
makeTransaction :: [TxIn]                                               -- ^ The UTxOs to be spent.
                -> [TxOut MaryEra]                                      -- ^ The output UTxOs.
                -> Maybe SlotNo                                         -- ^ The latest slot for the transaction.
                -> Maybe TxMetadata                                     -- ^ The metadata.
                -> Maybe (PolicyId, SimpleScript SimpleScriptV2, Value) -- ^ The value to be minted.
                -> TxBodyContent BuildTx MaryEra                        -- ^ Action for building the transaction.
makeTransaction txIns' txOuts before metadata minting =
  let
    txIns             = (, BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> txIns'
    txInsCollateral   = TxInsCollateralNone
    txFee             = TxFeeExplicit explicitFees 0
    txValidityRange   = (
                          TxValidityNoLowerBound
                        , maybe
                            (TxValidityNoUpperBound supportedNoUpperBound)
                            (TxValidityUpperBound supportedUpperBound)
                            before
                        )
    txMetadata        = maybe TxMetadataNone (TxMetadataInEra TxMetadataInMaryEra) metadata
    txAuxScripts      = TxAuxScriptsNone
    txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    txExtraKeyWits    = TxExtraKeyWitnessesNone
    txProtocolParams  = BuildTxWith Nothing
    txWithdrawals     = TxWithdrawalsNone
    txCertificates    = TxCertificatesNone
    txUpdateProposal  = TxUpdateProposalNone
    txMintValue       = maybe
                          TxMintNone
                          ( \(policy, script, value) ->
                            TxMintValue supportedMultiAsset value
                              . BuildTxWith
                              . M.singleton policy
                              $ SimpleScriptWitness SimpleScriptV2InMary SimpleScriptV2 script
                          )
                          minting
  in
    TxBodyContent{..}


-- | Include the fee in a transaction.
includeFee :: MonadFail m
           => MonadIO m
           => NetworkId                                 -- ^ The network.
           -> ProtocolParameters                        -- ^ The protocol parameters.
           -> Int                                       -- ^ The number of inputs.
           -> Int                                       -- ^ The number of outputs.
           -> Int                                       -- ^ The number of Shelley witnesses.
           -> Int                                       -- ^ The number of Byron witnesses.
           -> TxBodyContent BuildTx MaryEra             -- ^ The transaction body.
           -> MantisM m (TxBodyContent BuildTx MaryEra) -- ^ Action for the transaction body with fee included.
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
    [TxOut addr (TxOutValue s value) _] <- return $ txOuts content
    let
      fee = negateValue $ lovelaceToValue lovelace
    return
      $ content
        {
          txFee  = TxFeeExplicit explicitFees lovelace
        , txOuts = [TxOut addr (TxOutValue s $ value <> fee) TxOutDatumHashNone]
        }


-- | Read JSON metadata from a file.
readMetadata' :: MonadIO m
              => FilePath                        -- ^ Path to the metadata file.
              -> MantisM m (A.Value, TxMetadata) -- ^ Action for reading the file as JSON and metadata.
readMetadata' filename =
  do
    json <-
      foistMantisMaybeIO "Could not decode metadata."
        $ A.decodeFileStrict filename
    metadata <-
      foistMantisEither
        $ metadataFromJson TxMetadataJsonNoSchema json
    return (json, metadata)


-- | Read JSON metadata from a file.
readMetadata :: MonadIO m
             => FilePath             -- ^ Path to the metadata file.
             -> MantisM m TxMetadata -- ^ Action for reading the file as metadata.
readMetadata = fmap snd . readMetadata'


-- | Print information about a UTxO.
printUTxO :: MonadIO m
          => String       -- ^ How much to indent the output.
          -> UTxO MaryEra -- ^ The UTxO.
          -> MantisM m () -- ^ Action to print the information.
printUTxO indent (UTxO utxoMap) =
  sequence_
    [
      do
        liftIO
          . putStrLn
          $ indent ++ "Transaction: " ++ show' txhash  ++ "#" ++ show txin
        printValue (indent ++ "  ") value'
    |
      (TxIn (TxId txhash) txin, TxOut _ (TxOutValue _ value') _) <- M.assocs utxoMap
    ]


-- | Print a value.
printValue :: MonadIO m
           => String       -- ^ How much to indent the output.
           -> Value        -- ^ The value.
           -> MantisM m () -- ^ Action to print the information.
printValue = (liftIO .) . printValueIO


-- | Print a value.
printValueIO :: String
             -> Value -- ^ The value.
             -> IO () -- ^ Action to print the information.
printValueIO indent value =
  do
    putStrLn $ indent ++ show (selectLovelace value)
    sequence_
      [
        putStrLn $ indent ++ show quantity ++ "  " ++ show' policy ++ "." ++ show' asset
      |
        (AssetId (PolicyId policy) (AssetName asset), quantity) <- valueToList $ filterValue (/= AdaAssetId) value
      ]


-- | Strip a leading and trailing quotation mark when showing a string.
show' :: Show a
      => a      -- ^ The value.
      -> String -- ^ The string represenation.
show' = init . tail . show


-- | Summarize the values in a UTxO.
summarizeValues :: UTxO MaryEra -- ^ The UTxO.
                -> (Int, Value) -- ^ The number of values and their total.
summarizeValues (UTxO utxoMap) =
  let
    values =
      [
        value'
      |
        (_, TxOut _ (TxOutValue _ value') _) <- M.assocs utxoMap
      ]
  in
    (length values, mconcat values)


-- | Prepare a minting script.
makeMinting :: Value                               -- ^ The value to which minting will be added.
            -> String                              -- ^ The asset name.
            -> Integer                             -- ^ The number of tokens to omit.
            -> Hash PaymentKey                     -- ^ Hash of the payment key.
            -> Maybe SlotNo                        -- ^ The last slot number for minting.
            -> (SimpleScript SimpleScriptV2, Value, Value) -- ^ The minting script, value minted, and total value.
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


-- | Prepare for minting from a JSON file specifying NFTs.
readMinting :: MonadFail m
            => MonadIO m
            => PolicyId                               -- ^ The policy ID.
            -> FilePath                               -- ^ Path to the metadata file.
            -> MantisM m (A.Value, TxMetadata, Value) -- ^ Action reading the metadata file and returning the JSON, metadata, and value for minting.
readMinting policyId filename =
  do
    A.Object json <-
      foistMantisMaybeIO "Could not decode metadata."
        $ A.decodeFileStrict filename
    let
      json' =
        A.Object
        . H.singleton "721"
        . A.Object
        $ H.singleton
          (T.pack . BS.unpack $ serialiseToRawBytesHex policyId)
          (A.Object json)
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
    metadata <-
      foistMantisEither
        $ metadataFromJson TxMetadataJsonNoSchema json'
    return (json', metadata, minting)
