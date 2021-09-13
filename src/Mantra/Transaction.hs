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


module Mantra.Transaction (
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
) where


import Cardano.Api (AssetId(..), AssetName(..), BuildTx, BuildTxWith(..), Hash, IsCardanoEra(..), IsShelleyBasedEra, KeyWitnessInCtx(..), Lovelace, NetworkId, PaymentKey, PolicyId(..), Quantity(..), ScriptLanguage(..), SimpleScript(..), SimpleScriptV2, SimpleScriptVersion(..), ScriptWitness(..), SlotNo(..), TxAuxScripts(..), TxCertificates(..), TxExtraScriptData(..), TxExtraKeyWitnesses(..), TxFee(..), TxInsCollateral(..), TxIx(..), TxMetadata, TxMetadataInEra(..), TxMetadataJsonSchema(..), TxMintValue(..), TxOutDatumHash(..), TxScriptValidity(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), Value, Witness(..), estimateTransactionFee, filterValue, lovelaceToValue, makeSignedTransaction, makeTransactionBody, metadataFromJson, multiAssetSupportedInEra, negateValue, scriptLanguageSupportedInEra, selectLovelace, serialiseToRawBytesHex, txFeesExplicitInEra, txMetadataSupportedInEra, validityNoUpperBoundSupportedInEra, validityUpperBoundSupportedInEra, valueFromList, valueToList)
import Cardano.Api.Shelley (ProtocolParameters, TxBodyContent(..), TxId(..), TxIn(..), TxOut(..), TxOutValue(..), UTxO(..), protocolParamTxFeeFixed, protocolParamTxFeePerByte)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantra.Script (mintingScript)
import Mantra.Types (MantraM, foistMantraEither, foistMantraMaybeIO)

import qualified Data.Aeson            as A  (Value(..), decodeFileStrict)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.HashMap.Strict   as H  (keys, singleton)
import qualified Data.Map.Strict       as M  (assocs, singleton)
import qualified Data.Text             as T  (pack, unpack)


-- | Make a valid fee.
validFee :: IsCardanoEra era
         => Lovelace  -- ^ The amount of lovelace.
         -> TxFee era -- ^ The fee for the era.
validFee fee =
  either
    TxFeeImplicit
    (`TxFeeExplicit` fee)
    $ txFeesExplicitInEra cardanoEra


-- | Make a valid range of slots.
validRange :: IsCardanoEra era
           => Maybe SlotNo                                         -- ^ The upper bound, if any.
           -> (TxValidityLowerBound era, TxValidityUpperBound era) -- ^ The range for the era.
validRange Nothing =
  let
    Just supported = validityNoUpperBoundSupportedInEra cardanoEra
  in
    (
      TxValidityNoLowerBound
    , TxValidityNoUpperBound supported
    )
validRange (Just before) =
  let
    Just notSupported = validityNoUpperBoundSupportedInEra cardanoEra
    supported = validityUpperBoundSupportedInEra cardanoEra
  in
    (
      TxValidityNoLowerBound
    , maybe
        (TxValidityNoUpperBound notSupported)
        (`TxValidityUpperBound` before)
        supported
    )


-- | Make valid metadata.
validMetadata :: IsCardanoEra era
              => Maybe TxMetadata    -- ^ The metadata, if any.
              -> TxMetadataInEra era -- ^ The metadata for the era.
validMetadata Nothing = TxMetadataNone
validMetadata (Just metadata) =
  maybe
    TxMetadataNone
    (`TxMetadataInEra` metadata)
    $ txMetadataSupportedInEra cardanoEra


validMint :: IsCardanoEra era
          => Maybe (PolicyId, SimpleScript SimpleScriptV2, Value) -- ^ The policy, script, and value, if any.
          -> TxMintValue BuildTx era                              -- ^ The minting for the era.
validMint Nothing = TxMintNone
validMint (Just (policy, script, value)) =
  case (multiAssetSupportedInEra cardanoEra, scriptLanguageSupportedInEra cardanoEra (SimpleScriptLanguage SimpleScriptV2)) of
    (Right supportedMultiAsset, Just supportedScript) -> TxMintValue supportedMultiAsset value
                                                           . BuildTxWith
                                                           . M.singleton policy
                                                           $ SimpleScriptWitness supportedScript SimpleScriptV2 script
    _                                                 -> TxMintNone


-- | Build a transaction.
makeTransaction :: IsCardanoEra era
                => [TxIn]                                               -- ^ The UTxOs to be spent.
                -> [TxOut era]                                          -- ^ The output UTxOs.
                -> Maybe SlotNo                                         -- ^ The latest slot for the transaction.
                -> Maybe TxMetadata                                     -- ^ The metadata.
                -> Maybe (PolicyId, SimpleScript SimpleScriptV2, Value) -- ^ The value to be minted.
                -> TxBodyContent BuildTx era                            -- ^ Action for building the transaction.
makeTransaction txIns' txOuts before metadata minting =
  let
    txIns             = (, BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> txIns'
    txInsCollateral   = TxInsCollateralNone
    txFee             = validFee 0
    txValidityRange   = validRange before
    txMetadata        = validMetadata metadata
    txAuxScripts      = TxAuxScriptsNone
    txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    txExtraKeyWits    = TxExtraKeyWitnessesNone
    txProtocolParams  = BuildTxWith Nothing
    txWithdrawals     = TxWithdrawalsNone
    txCertificates    = TxCertificatesNone
    txUpdateProposal  = TxUpdateProposalNone
    txMintValue       = validMint minting
    txScriptValidity  = TxScriptValidityNone
  in
    TxBodyContent{..}


-- | Include the fee in a transaction.
includeFee :: IsShelleyBasedEra era
           => MonadFail m
           => MonadIO m
           => NetworkId                             -- ^ The network.
           -> ProtocolParameters                    -- ^ The protocol parameters.
           -> Int                                   -- ^ The number of inputs.
           -> Int                                   -- ^ The number of outputs.
           -> Int                                   -- ^ The number of Shelley witnesses.
           -> Int                                   -- ^ The number of Byron witnesses.
           -> TxBodyContent BuildTx era             -- ^ The transaction body.
           -> MantraM m (TxBodyContent BuildTx era) -- ^ Action for the transaction body with fee included.
includeFee network pparams nIn nOut nShelley nByron content =
  do
    body <- foistMantraEither $ makeTransactionBody content
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
          txFee  = validFee lovelace
        , txOuts = [TxOut addr (TxOutValue s $ value <> fee) TxOutDatumHashNone]
        }


-- | Read JSON metadata from a file.
readMetadata' :: MonadIO m
              => FilePath                        -- ^ Path to the metadata file.
              -> MantraM m (A.Value, TxMetadata) -- ^ Action for reading the file as JSON and metadata.
readMetadata' filename =
  do
    json <-
      foistMantraMaybeIO "Could not decode metadata."
        $ A.decodeFileStrict filename
    metadata <-
      foistMantraEither
        $ metadataFromJson TxMetadataJsonNoSchema json
    return (json, metadata)


-- | Read JSON metadata from a file.
readMetadata :: MonadIO m
             => FilePath             -- ^ Path to the metadata file.
             -> MantraM m TxMetadata -- ^ Action for reading the file as metadata.
readMetadata = fmap snd . readMetadata'


-- | Print information about a UTxO.
printUTxO :: MonadIO m
          => String       -- ^ How much to indent the output.
          -> UTxO era     -- ^ The UTxO.
          -> MantraM m () -- ^ Action to print the information.
printUTxO indent (UTxO utxoMap) =
  sequence_
    [
      do
        liftIO
          . putStrLn
          $ indent ++ "Transaction: " ++ show' txhash  ++ "#" ++ show txix
        printValue (indent ++ "  ") value'
    |
      (TxIn (TxId txhash) (TxIx txix), TxOut _ (TxOutValue _ value') _) <- M.assocs utxoMap
    ]


-- | Print a value.
printValue :: MonadIO m
           => String       -- ^ How much to indent the output.
           -> Value        -- ^ The value.
           -> MantraM m () -- ^ Action to print the information.
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
summarizeValues :: UTxO era     -- ^ The UTxO.
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
            -> ((PolicyId, SimpleScript SimpleScriptV2, Value), Value) -- ^ The minting script, value minted, and total value.
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
      (PolicyId scriptHash, script, minting)
    , value <> minting
    )


-- | Prepare for minting from a JSON file specifying NFTs.
readMinting :: MonadFail m
            => MonadIO m
            => PolicyId                               -- ^ The policy ID.
            -> FilePath                               -- ^ Path to the metadata file.
            -> MantraM m (A.Value, TxMetadata, Value) -- ^ Action reading the metadata file and returning the JSON, metadata, and value for minting.
readMinting policyId filename =
  do
    A.Object json <-
      foistMantraMaybeIO "Could not decode metadata."
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
      foistMantraEither
        $ metadataFromJson TxMetadataJsonNoSchema json'
    return (json', metadata, minting)
