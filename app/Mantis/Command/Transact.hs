
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command.Transact (
  command
, main
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Eras (CardanoEra(MaryEra))
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), fromMaryValue, TxOutValue(..), makeScriptWitness, makeSignedTransaction, makeShelleyKeyWitness)
import Cardano.Api.Typed (NetworkMagic(..), anyAddressInEra, makeTransactionBody)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe, maybeToList)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Transaction (fromShelleyUTxO, includeFee, makeMinting, makeTransaction, printUTxO, printValue, readMetadata, summarizeValues, supportedMultiAsset)
import Mantis.Types (MantisM, foistMantisEither)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)

import qualified Cardano.Chain.Slotting     as Chain (EpochSlots(..))
import qualified Data.ByteString.Lazy.Char8 as LBS   (unpack, writeFile)
import qualified Data.Map.Strict            as M     (keys)
import qualified Options.Applicative        as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "transact"
    $ O.info options (O.progDesc "Submit Cardano metadata or mint Cardano tokens.")


options :: O.Parser Mantis
options =
  Transact
    <$>             O.strArgument   (                      O.metavar "CONFIG_FILE"   <> O.help "Path to configuration file."                                                               )
    <*> O.optional (O.strArgument   $                      O.metavar "TOKEN"         <> O.help "Name of token to mint or burn."                                                            )
    <*> O.optional (O.option O.auto $ O.long "count"    <> O.metavar "INTEGER"       <> O.help "Number of tokens to mint or burn."                                                         )
    <*> O.optional (O.option O.auto $ O.long "expires"  <> O.metavar "SLOT"          <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")
    <*> O.optional (O.strOption     $ O.long "output"   <> O.metavar "ADDRESS"       <> O.help "Address for output of transaction."                                                        )
    <*> O.optional (O.strOption     $ O.long "script"   <> O.metavar "SCRIPT_FILE"   <> O.help "Path to output script JSON file."                                                          )
    <*> O.optional (O.strOption     $ O.long "metadata" <> O.metavar "METADATA_FILE" <> O.help "Path to metadata JSON file."                                                               )


main :: MonadFail m
     => MonadIO m
     => FilePath
     -> Maybe String
     -> Maybe Integer
     -> Maybe SlotRef
     -> Maybe String
     -> Maybe FilePath
     -> Maybe FilePath
     -> MantisM m ()
main configFile tokenName tokenCount tokenSlot outputAddress scriptFile metadataFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    let
      protocol = CardanoProtocol $ Chain.EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Network: " ++ show network

    tip <- queryTip protocol network
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Tip: " ++ show tip
    let
      before = (`adjustSlot` tip) <$> tokenSlot

    pparams <- queryProtocol protocol network
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    address <- readAddress addressString
    address' <- readAddress $ fromMaybe addressString outputAddress
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Input Address: " ++ addressString
    liftIO . putStrLn $ "Output Address: " ++ fromMaybe addressString outputAddress

    verificationKey <- readVerificationKey verificationKeyFile
    verificationKeyHash <- makeVerificationKeyHash verificationKey
    signingKey <- readSigningKey signingKeyFile
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Verification key hash: " ++ show verificationKeyHash
    liftIO $ putStrLn "Signing key . . . read successfuly."

    liftIO $ putStrLn ""
    liftIO $ putStrLn "Unspect UTxO:"
    utxo <- queryUTxO protocol address network
    printUTxO "  " utxo

    let
      (nIn, value) = summarizeValues utxo
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Total value:"
    printValue "  " value

    metadata <- sequence $ readMetadata <$> metadataFile
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Metadata . . . read and parsed."

    let
      (script, minting, value') =
        case tokenName of
          Nothing   -> (Nothing, Nothing, fromMaryValue value)
          Just name -> let
                         (script', minting', value'') = makeMinting
                                                          value
                                                          name
                                                          (fromMaybe 1 tokenCount)
                                                          verificationKeyHash
                                                          before
                       in
                         (Just script', Just minting', value'')
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Policy: " ++ show script
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Minting: " ++ show minting

    liftIO
      $ whenJust scriptFile
       (`LBS.writeFile` encodePretty script)

    let
      Just address'' = anyAddressInEra MaryEra address'
    txBody <- includeFee network pparams nIn 1 1 0
      $ makeTransaction 
        (M.keys $ fromShelleyUTxO utxo)
        [TxOut address'' (TxOutValue supportedMultiAsset value')]
        before
        metadata
        Nothing
        minting
    txRaw <- foistMantisEither $ makeTransactionBody txBody
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      witness' = makeScriptWitness <$> script
      txSigned = makeSignedTransaction (witness : maybeToList witness') txRaw
    result <- submitTransaction protocol network txSigned
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Result: " ++ show result
    liftIO . putStrLn $ "TxID: " ++ show (getTxId txRaw)

