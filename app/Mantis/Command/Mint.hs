
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command.Mint (
  command
, main
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Eras (CardanoEra(MaryEra))
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), fromMaryValue, TxOutValue(..), makeScriptWitness, makeSignedTransaction, makeShelleyKeyWitness)
import Cardano.Api.Typed (NetworkMagic(..), PolicyId(..), anyAddressInEra, makeTransactionBody)
import Control.Monad.Except (runExceptT)
import Control.Monad.Extra (whenJust)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Script (mintingScript)
import Mantis.Transaction (fromShelleyUTxO, includeFee, makeTransaction, printUTxO, printValue, readMinting, summarizeValues, supportedMultiAsset)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)

import qualified Cardano.Chain.Slotting     as Chain (EpochSlots(..))
import qualified Data.ByteString.Lazy.Char8 as LBS   (unpack, writeFile)
import qualified Data.Map.Strict            as M     (keys)
import qualified Options.Applicative        as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "mint"
    $ O.info options (O.progDesc "Mint batches of Cardano non-fungible tokens.")


options :: O.Parser Mantis
options =
  Mint
    <$>             O.strArgument   (                      O.metavar "CONFIG_FILE"   <> O.help "Path to configuration file."                                                               )
    <*>             O.strArgument   (                      O.metavar "MINTING_FILE"  <> O.help "Path to minting JSON file."                                                                )
    <*> O.optional (O.option O.auto $ O.long "expires"  <> O.metavar "SLOT"          <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")
    <*> O.optional (O.strOption     $ O.long "script"   <> O.metavar "SCRIPT_FILE"   <> O.help "Path to output script JSON file."                                                          )
    <*> O.optional (O.strOption     $ O.long "metadata" <> O.metavar "METADATA_FILE" <> O.help "Path to output metadata JSON file."                                                        )


main :: FilePath
     -> FilePath
     -> Maybe SlotRef
     -> Maybe FilePath
     -> Maybe FilePath
     -> IO ()
main configFile mintingFile tokenSlot scriptFile metadataFile =
  do
    Configuration{..} <- read <$> readFile configFile

    let
      protocol = CardanoProtocol $ Chain.EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    putStrLn ""
    putStrLn $ "Network: " ++ show network

    Right tip <- runExceptT $ queryTip protocol network
    putStrLn ""
    putStrLn $ "Tip: " ++ show tip
    let
      before = (`adjustSlot` tip) <$> tokenSlot

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
    putStrLn "Signing key . . . read successfuly."

    putStrLn ""
    putStrLn "Unspect UTxO:"
    Right utxo <- runExceptT $ queryUTxO protocol address network
    printUTxO "  " utxo

    let
      (nIn, value) = summarizeValues utxo
    putStrLn ""
    putStrLn "Total value:"
    printValue "  " value

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    putStrLn ""
    putStrLn $ "Policy ID: " ++ show scriptHash
    putStrLn $ "Policy: " ++ show script
    whenJust scriptFile
      (`LBS.writeFile` encodePretty script)

    (json, metadata, minting) <- readMinting (PolicyId scriptHash) mintingFile
    let
      value' = fromMaryValue value <> minting
    putStrLn ""
    putStrLn "Metadata . . . read and parsed."
    whenJust metadataFile
      (`LBS.writeFile` encodePretty json)
    putStrLn ""
    putStrLn $ "Minting: " ++ show minting

    let
      Just address' = anyAddressInEra MaryEra address
      txBody = includeFee network pparams nIn 1 1 0
        $ makeTransaction 
          (M.keys $ fromShelleyUTxO utxo)
          [TxOut address' (TxOutValue supportedMultiAsset value')]
          before
          (Just metadata)
          Nothing
          (Just minting)
      Right txRaw = makeTransactionBody txBody
    putStrLn ""
    putStrLn $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      witness' = makeScriptWitness script
      txSigned = makeSignedTransaction [witness, witness'] txRaw
    print witness
    result <- runExceptT $ submitTransaction protocol network txSigned
    putStrLn ""
    putStrLn $ "Result: " ++ show result
    putStrLn $ "TxID: " ++ show (getTxId txRaw)
