
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where

import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Eras (CardanoEra(MaryEra))
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), TxOutValue(..), fromMaryValue, makeScriptWitness, makeSignedTransaction, makeShelleyKeyWitness)
import Cardano.Api.Typed (AssetId(..), AssetName(..), NetworkMagic(..), PolicyId(..), Quantity(..), SlotNo(..), anyAddressInEra, makeTransactionBody, valueFromList)
import Control.Monad.Except (runExceptT)
import Data.Aeson (encode)
import Data.Maybe (maybeToList)
import Data.Version (showVersion)
import Data.Word (Word32)
import Mantis.Query (queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Script (mintingScript)
import Mantis.Transaction (fromShelleyUTxO, includeFee, makeTransaction, readMetadata, supportedMultiAsset)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import Paths_mantis (version)

import qualified Cardano.Chain.Slotting     as Chain   (EpochSlots(..))
import qualified Cardano.Ledger.Mary.Value  as Mary    (AssetName(..), PolicyID(..), Value(..))
import qualified Data.ByteString.Char8      as BS      (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS     (unpack)
import qualified Data.Map.Strict            as M       (assocs, keys)
import qualified Options.Applicative        as O
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
  }
    deriving (Read, Show)


data Mantis = Mantis
  {
    configFile   :: FilePath
  , metadataFile :: Maybe FilePath
  , tokenName    :: Maybe String
  , tokenCount   :: Integer
  , tokenSlot    :: Maybe Int
  }


main :: IO ()
main =
  do
    let
      parser =
        O.info
          (O.helper <*> versionOption <*> programOptions)
          (O.fullDesc <> O.progDesc "Submit Cardano metadata or mint Cardano tokens." <> O.header "Mantis Cardano tool.")
      versionOption = O.infoOption ("Mantis " ++ showVersion version) (O.long "version" <> O.help "Show version.")
      programOptions =
        Mantis
          <$>             O.strArgument   ( O.metavar "CONFIG"    <>              O.help "Path to configuration file."                       )
          <*> O.optional (O.strArgument   $ O.metavar "METADATA"  <>              O.help "Path to metadata JSON file."                       )
          <*> O.optional (O.strArgument   $ O.metavar "TOKEN"     <>              O.help "Name of token to mint."                            )
          <*>             O.option O.auto ( O.long    "count"     <> O.value 1 <> O.help "Number of tokens to mint."                         )
          <*> O.optional (O.option O.auto $ O.long    "before"    <>              O.help "Relative number of slots when tokens are mintable.")
    Mantis{..} <- O.execParser parser

    Configuration{..} <- read <$> readFile configFile
    let
      protocol = CardanoProtocol $ Chain.EpochSlots 21600
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    putStrLn ""
    putStrLn $ "Network: " ++ show network

    Right (SlotNo tip) <- runExceptT $ queryTip protocol network
    putStrLn ""
    putStrLn $ "Tip slot: " ++ show tip

    Right pparams <- runExceptT $ queryProtocol protocol network
    putStrLn ""
    putStrLn $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    let
      Just address = readAddress addressString
    putStrLn ""
    putStrLn $ "Address: " ++ addressString

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    signingKey <- readSigningKey signingKeyFile
    putStrLn ""
    putStrLn $ "Verification key hash: " ++ show verificationKeyHash
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
      (script, minting, value') =
        case tokenName of
          Nothing   -> (Nothing, Nothing, fromMaryValue value)
          Just name -> let
                         before = SlotNo . (tip +) . toEnum <$> tokenSlot
                         (script', hash) = mintingScript verificationKeyHash before
                         minting' = valueFromList
                           [(
                             AssetId (PolicyId hash) (AssetName $ BS.pack name)
                           , Quantity tokenCount
                           )]
                       in
                         (
                           Just script'
                         , Just minting'
                         , fromMaryValue value <> minting'
                         )
    putStrLn ""
    putStrLn $ "Policy: " ++ show script
    putStrLn ""
    putStrLn $ "Minting: " ++ show minting

    let
      Just address' = anyAddressInEra MaryEra address
      txBody = includeFee network pparams nIn 1 1 0
        $ makeTransaction 
          (M.keys $ fromShelleyUTxO utxo)
          [TxOut address' (TxOutValue supportedMultiAsset value')]
          metadata
          Nothing
          minting
      Right txRaw = makeTransactionBody txBody
    putStrLn ""
    putStrLn $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      witness' = makeScriptWitness <$> script
      txSigned = makeSignedTransaction (witness : maybeToList witness') txRaw
    print witness
    result <- runExceptT $ submitTransaction protocol network txSigned
    putStrLn ""
    putStrLn $ "Result: " ++ show result
    putStrLn $ "TxID: " ++ show (getTxId txRaw)
