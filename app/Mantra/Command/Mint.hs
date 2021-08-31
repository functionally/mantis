
{-# LANGUAGE RecordWildCards   #-}


module Mantra.Command.Mint (
  command
, main
) where


import Cardano.Api (ConsensusModeParams(CardanoModeParams), EpochSlots(..), IsShelleyBasedEra, NetworkId(..), NetworkMagic(..), PolicyId(..), ShelleyBasedEra, TxOutDatumHash(..), anyAddressInEra, getTxId, makeTransactionBody, multiAssetSupportedInEra, shelleyBasedToCardanoEra)
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), TxOutValue(..), UTxO(..), makeSignedTransaction, makeShelleyKeyWitness)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Mantra.Command.Types (Configuration(..), Mantra(..))
import Mantra.Query (adjustSlot, queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantra.Script (mintingScript)
import Mantra.Transaction (includeFee, makeTransaction, printUTxO, printValue, readMinting, summarizeValues)
import Mantra.Types (MantraM, SlotRef, foistMantraEither, printMantra)
import Mantra.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, writeFile)
import qualified Data.Map.Strict            as M   (keys)
import qualified Options.Applicative        as O


command :: O.Mod O.CommandFields Mantra
command =
  O.command "mint"
    $ O.info options (O.progDesc "Mint batches of Cardano non-fungible tokens.")


options :: O.Parser Mantra
options =
  Mint
    <$>             O.strArgument   (                      O.metavar "CONFIG_FILE"   <> O.help "Path to configuration file."                                                               )
    <*>             O.strArgument   (                      O.metavar "MINTING_FILE"  <> O.help "Path to minting JSON file."                                                                )
    <*> O.optional (O.option O.auto $ O.long "expires"  <> O.metavar "SLOT"          <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")
    <*> O.optional (O.strOption     $ O.long "output"   <> O.metavar "ADDRESS"       <> O.help "Address for output of transaction."                                                        )
    <*> O.optional (O.strOption     $ O.long "script"   <> O.metavar "SCRIPT_FILE"   <> O.help "Path to output script JSON file."                                                          )
    <*> O.optional (O.strOption     $ O.long "metadata" <> O.metavar "METADATA_FILE" <> O.help "Path to output metadata JSON file."                                                        )


main :: IsShelleyBasedEra era
     => MonadFail m
     => MonadIO m
     => ShelleyBasedEra era
     -> (String -> MantraM m ())
     -> FilePath
     -> FilePath
     -> Maybe SlotRef
     -> Maybe String
     -> Maybe FilePath
     -> Maybe FilePath
     -> MantraM m ()
main sbe debugMantra configFile mintingFile tokenSlot outputAddress scriptFile metadataFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    let
      era = shelleyBasedToCardanoEra sbe
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantra ""
    debugMantra $ "Network: " ++ show network

    tip <- queryTip socketPath protocol network
    debugMantra ""
    debugMantra $ "Tip: " ++ show tip
    let
      before = (`adjustSlot` tip) <$> tokenSlot

    pparams <- queryProtocol sbe socketPath protocol network
    debugMantra ""
    debugMantra $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    address <- readAddress addressString
    address' <- readAddress $ fromMaybe addressString outputAddress
    debugMantra ""
    debugMantra $ "Input Address: " ++ addressString
    debugMantra $ "Output Address: " ++ fromMaybe addressString outputAddress

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    signingKey <- readSigningKey signingKeyFile
    debugMantra ""
    debugMantra $ "Verification key hash: " ++ show verificationKeyHash
    debugMantra "Signing key . . . read successfuly."

    debugMantra ""
    debugMantra "Unspect UTxO:"
    utxo@(UTxO utxo') <- queryUTxO sbe socketPath protocol address network
    printUTxO "  " utxo

    let
      (nIn, value) = summarizeValues utxo
    debugMantra ""
    debugMantra "Total value:"
    printValue "  " value

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    debugMantra ""
    debugMantra $ "Policy ID: " ++ show scriptHash
    debugMantra $ "Policy: " ++ show script
    liftIO
      $ whenJust scriptFile
        (`LBS.writeFile` encodePretty script)

    (json, metadata, minting) <- readMinting (PolicyId scriptHash) mintingFile
    let
      value' = value <> minting
    debugMantra ""
    debugMantra "Metadata . . . read and parsed."
    liftIO
      $ whenJust metadataFile
        (`LBS.writeFile` encodePretty json)
    debugMantra ""
    debugMantra $ "Minting: " ++ show minting

    let
      Just address'' = anyAddressInEra era address'
      Right supportedMultiAsset = multiAssetSupportedInEra era
    txBody <- includeFee network pparams nIn 1 1 0
      $ makeTransaction
        (M.keys utxo')
        [TxOut address'' (TxOutValue supportedMultiAsset value') TxOutDatumHashNone]
        before
        (Just metadata)
        (Just (PolicyId scriptHash, script, minting))
    txRaw <- foistMantraEither $ makeTransactionBody txBody
    debugMantra ""
    debugMantra $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ either WitnessPaymentKey WitnessPaymentExtendedKey signingKey
      txSigned = makeSignedTransaction [witness] txRaw
    result <- submitTransaction sbe socketPath protocol network txSigned
    printMantra ""
    case result of
      SubmitSuccess     -> printMantra $ "Success: " ++ show (getTxId txRaw)
      SubmitFail reason -> printMantra $ "Failure: " ++ show reason
