
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Eras (CardanoEra(MaryEra))
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), TxOutValue(..), fromMaryValue, makeSignedTransaction, makeShelleyKeyWitness)
import Cardano.Api.Typed (NetworkMagic(..), anyAddressInEra, makeTransactionBody)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.Aeson (encode)
import Data.Word (Word32)
import Mantis.Query (queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Transaction (fromShelleyUTxO, includeFee, makeTransaction, readMetadata, supportedMultiAsset)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import System.Environment (getArgs)

import qualified Cardano.Chain.Slotting     as Chain   (EpochSlots(..))
import qualified Cardano.Ledger.Mary.Value  as Mary    (AssetName(..), PolicyID(..), Value(..))
import qualified Data.ByteString.Lazy.Char8 as LBS     (unpack)
import qualified Data.Map.Strict            as M       (assocs, keys)
import qualified Shelley.Spec.Ledger.API    as Shelley (ScriptHash(..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley (TxId(..), TxIn(..), TxOut(..))
import qualified Shelley.Spec.Ledger.UTxO   as Shelley (UTxO(..))


show' :: Show a => a -> String
show' = init . tail . show


data Configuration =
  Configuration
  {
    magic               :: Maybe Word32
  , addressString       :: String
  , verificationKeyFile :: FilePath
  , signingKeyFile      :: FilePath
  , metadataFile        :: Maybe FilePath
  }
    deriving (Read, Show)


main :: IO ()
main =
  do

    void getArgs
    let configFile = "testnet.config"
    Configuration{..} <- read <$> readFile configFile

    let
      protocol = CardanoProtocol $ Chain.EpochSlots 21600
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    putStrLn ""
    putStrLn $ "Network: " ++ show network

    Right tip <- runExceptT $ queryTip protocol network
    putStrLn ""
    putStrLn $ "Tip: " ++ show tip

    Right pparams <- runExceptT $ queryProtocol protocol network
    putStrLn ""
    putStrLn $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    let
      Just address = readAddress addressString
    putStrLn ""
    putStrLn $ "Address: " ++ addressString

    verificationKey <- readVerificationKey verificationKeyFile
    signingKey <- readSigningKey signingKeyFile
    putStrLn ""
    putStrLn $ "Verification key hash: " ++ show (makeVerificationKeyHash verificationKey)
    putStrLn "Signing key: read successfuly."

    putStrLn ""
    putStrLn "Unspect UTxO:"
    Right utxo@(Shelley.UTxO utxoMap) <- runExceptT $ queryUTxO protocol address network
    sequence_
      [
        do
          putStrLn $ "  Transaction: " ++ show' txhash  ++ "#" ++ show txin
          putStrLn $ "    " ++ show lovelace ++ " Lovelace"
          sequence_
            [
              putStrLn $ "    " ++ show quantity ++ "  " ++ show' policy ++ "." ++ show' asset
            |
              (Mary.PolicyID (Shelley.ScriptHash policy), assets) <- M.assocs policies
            , (Mary.AssetName asset, quantity) <- M.assocs assets
            ]
      |
        (Shelley.TxIn (Shelley.TxId txhash) txin, Shelley.TxOut _ value') <- M.assocs utxoMap
      , let Mary.Value lovelace policies = value'
      ]

    let
      values =  
        [
          value'
        |
          (_, Shelley.TxOut _ value') <- M.assocs utxoMap
        ]
      nIn = length values
      value = mconcat values
      Mary.Value lovelace policies = value
    putStrLn ""
    putStrLn "Total value:"
    putStrLn $ "  " ++ show lovelace ++ " Lovelace"
    sequence_
      [
        putStrLn $ "  " ++ show quantity ++ "  " ++ show' policy ++ "." ++ show' asset
      |
        (Mary.PolicyID (Shelley.ScriptHash policy), assets) <- M.assocs policies
      , (Mary.AssetName asset, quantity) <- M.assocs assets
      ]

    metadata <- sequence $ readMetadata <$> metadataFile
    putStrLn ""
    putStrLn "Metadata: read and parsed."

    let
      Just address' = anyAddressInEra MaryEra address
      txBody = includeFee network pparams nIn 1 1 0
        $ makeTransaction 
          (M.keys $ fromShelleyUTxO utxo)
          [TxOut address' (TxOutValue supportedMultiAsset $ fromMaryValue value)]
          metadata
      Right txRaw = makeTransactionBody txBody
    putStrLn ""
    putStrLn $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      txSigned = makeSignedTransaction [witness] txRaw
    result <- runExceptT $ submitTransaction protocol network txSigned
    putStrLn ""
    putStrLn $ "Result: " ++ show result
    putStrLn $ "TxID: " ++ show (getTxId txRaw)
