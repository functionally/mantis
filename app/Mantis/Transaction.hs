
{-# LANGUAGE RecordWildCards #-}


module Mantis.Transaction (
  makeTransaction
, includeFee
, fromShelleyUTxO
, supportedMultiAsset
, readMetadata
) where


import Cardano.Api.Shelley (TxBodyContent(..), TxId(..), TxIn(..), TxOut(..), TxOutValue(..), fromMaryValue, fromShelleyAddr)
import Cardano.Api.Eras (CardanoEra(..), MaryEra, ShelleyLedgerEra)
import Cardano.Api.Typed (MultiAssetSupportedInEra(..), NetworkId, TxAuxScripts(..), TxCertificates(..), TxFee(..), TxFeesExplicitInEra(..), TxMetadata, TxMetadataInEra(..), TxMetadataSupportedInEra(..), TxMetadataJsonSchema(..), TxMintValue(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), ValidityNoUpperBoundSupportedInEra(..), estimateTransactionFee, lovelaceToValue, makeSignedTransaction, makeTransactionBody, metadataFromJson, multiAssetSupportedInEra, negateValue, txFeesExplicitInEra, validityNoUpperBoundSupportedInEra)
import Data.Aeson (decodeFileStrict)

import qualified Data.Map.Strict                  as M (Map, assocs, fromList)
import qualified Ouroboros.Consensus.Shelley.Eras as Ouroboros (StandardMary)
import qualified Shelley.Spec.Ledger.PParams      as Shelley (PParams)
import qualified Shelley.Spec.Ledger.API          as Shelley (PParams'(..), TxId(..), TxIn(..), TxOut(..), UTxO(..))


supportedUpperBound :: ValidityNoUpperBoundSupportedInEra MaryEra
Just supportedUpperBound = validityNoUpperBoundSupportedInEra MaryEra


supportedMultiAsset :: MultiAssetSupportedInEra MaryEra
Right supportedMultiAsset = multiAssetSupportedInEra MaryEra


explicitFees :: TxFeesExplicitInEra MaryEra
Right explicitFees = txFeesExplicitInEra MaryEra


makeTransaction :: [TxIn] -> [TxOut MaryEra] -> Maybe TxMetadata -> TxBodyContent MaryEra
makeTransaction txIns txOuts metadata=
  let
    txFee = TxFeeExplicit explicitFees 0
    txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound supportedUpperBound)
    txMetadata = maybe TxMetadataNone (TxMetadataInEra TxMetadataInMaryEra) metadata
    txAuxScripts = TxAuxScriptsNone
    txWithdrawals = TxWithdrawalsNone
    txCertificates = TxCertificatesNone
    txUpdateProposal = TxUpdateProposalNone
    txMintValue = TxMintNone
  in
    TxBodyContent{..}


includeFee :: NetworkId
           -> Shelley.PParams (ShelleyLedgerEra MaryEra)
           -> Int
           -> Int
           -> Int
           -> Int
           -> TxBodyContent MaryEra
           -> TxBodyContent MaryEra
includeFee network pparams nIn nOut nShelley nByron content =
  let
    Right tx = makeSignedTransaction [] <$> makeTransactionBody content
    lovelace = estimateTransactionFee
      network
      (Shelley._minfeeB pparams)
      (Shelley._minfeeA pparams)
      tx
      nIn nOut nShelley nByron
    TxOut addr (TxOutValue s value) : _ = txOuts content
    fee = negateValue $ lovelaceToValue lovelace
  in
    content
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


readMetadata :: FilePath -> IO TxMetadata
readMetadata filename =
  do
    Just json <- decodeFileStrict filename
    let
      Right metadata = metadataFromJson TxMetadataJsonNoSchema json
    return metadata
