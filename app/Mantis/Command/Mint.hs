
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command.Mint (
  command
, main
) where


import Cardano.Api (ConsensusModeParams(CardanoModeParams), CardanoEra(MaryEra), EpochSlots(..), NetworkId(..), NetworkMagic(..), PolicyId(..), anyAddressInEra, getTxId, makeTransactionBody)
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), TxOutValue(..), UTxO(..), makeScriptWitness, makeSignedTransaction, makeShelleyKeyWitness)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Script (mintingScript)
import Mantis.Transaction (includeFee, makeTransaction, printUTxO, printValue, readMinting, summarizeValues, supportedMultiAsset)
import Mantis.Types (MantisM, foistMantisEither, printMantis)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, writeFile)
import qualified Data.Map.Strict            as M   (keys)
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
    <*> O.optional (O.strOption     $ O.long "output"   <> O.metavar "ADDRESS"       <> O.help "Address for output of transaction."                                                        )
    <*> O.optional (O.strOption     $ O.long "script"   <> O.metavar "SCRIPT_FILE"   <> O.help "Path to output script JSON file."                                                          )
    <*> O.optional (O.strOption     $ O.long "metadata" <> O.metavar "METADATA_FILE" <> O.help "Path to output metadata JSON file."                                                        )


main :: MonadFail m
     => MonadIO m
     => (String -> MantisM m ())
     -> FilePath
     -> FilePath
     -> Maybe SlotRef
     -> Maybe String
     -> Maybe FilePath
     -> Maybe FilePath
     -> MantisM m ()
main debugMantis configFile mintingFile tokenSlot outputAddress scriptFile metadataFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantis ""
    debugMantis $ "Network: " ++ show network

    tip <- queryTip socketPath protocol network
    debugMantis ""
    debugMantis $ "Tip: " ++ show tip
    let
      before = (`adjustSlot` tip) <$> tokenSlot

    pparams <- queryProtocol socketPath protocol network
    debugMantis ""
    debugMantis $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    address <- readAddress addressString
    address' <- readAddress $ fromMaybe addressString outputAddress
    debugMantis ""
    debugMantis $ "Input Address: " ++ addressString
    debugMantis $ "Output Address: " ++ fromMaybe addressString outputAddress

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    signingKey <- readSigningKey signingKeyFile
    debugMantis ""
    debugMantis $ "Verification key hash: " ++ show verificationKeyHash
    debugMantis "Signing key . . . read successfuly."

    debugMantis ""
    debugMantis "Unspect UTxO:"
    utxo@(UTxO utxo') <- queryUTxO socketPath protocol address network
    printUTxO "  " utxo

    let
      (nIn, value) = summarizeValues utxo
    debugMantis ""
    debugMantis "Total value:"
    printValue "  " value

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    debugMantis ""
    debugMantis $ "Policy ID: " ++ show scriptHash
    debugMantis $ "Policy: " ++ show script
    liftIO
      $ whenJust scriptFile
        (`LBS.writeFile` encodePretty script)

    (json, metadata, minting) <- readMinting (PolicyId scriptHash) mintingFile
    let
      value' = value <> minting
    debugMantis ""
    debugMantis "Metadata . . . read and parsed."
    liftIO
      $ whenJust metadataFile
        (`LBS.writeFile` encodePretty json)
    debugMantis ""
    debugMantis $ "Minting: " ++ show minting

    let
      Just address'' = anyAddressInEra MaryEra address'
    txBody <- includeFee network pparams nIn 1 1 0
      $ makeTransaction 
        (M.keys utxo')
        [TxOut address'' (TxOutValue supportedMultiAsset value')]
        before
        (Just metadata)
        Nothing
        (Just minting)
    txRaw <- foistMantisEither $ makeTransactionBody txBody
    debugMantis ""
    debugMantis $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      witness' = makeScriptWitness script
      txSigned = makeSignedTransaction [witness, witness'] txRaw
    result <- submitTransaction socketPath protocol network txSigned
    printMantis ""
    case result of
      SubmitSuccess     -> printMantis $ "Success: " ++ show (getTxId txRaw)
      SubmitFail reason -> printMantis $ "Failure: " ++ show reason
