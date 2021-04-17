
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Script (
  command
, main
) where


import Cardano.Api (NetworkId(..))
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Typed (NetworkMagic(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryTip)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM)
import Mantis.Wallet (makeVerificationKeyHash, readVerificationKey)

import qualified Cardano.Chain.Slotting as Chain (EpochSlots(..))
import qualified Data.ByteString.Lazy   as LBS   (writeFile)
import qualified Options.Applicative    as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "script"
    $ O.info options (O.progDesc "Construct a minting script and compute its Policy ID.")


options :: O.Parser Mantis
options =
  Script
    <$>             O.strArgument   (                     O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                                               )
    <*> O.optional (O.option O.auto $ O.long "expires" <> O.metavar "SLOT"        <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")
    <*> O.optional (O.strOption     $ O.long "script"  <> O.metavar "SCRIPT_FILE" <> O.help "Path to output script JSON file."                                                          )


main :: MonadFail m
     => MonadIO m
     => FilePath
     -> Maybe SlotRef
     -> Maybe FilePath
     -> MantisM m ()
main configFile tokenSlot scriptFile =
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

    verificationKey <- readVerificationKey verificationKeyFile
    verificationKeyHash <- makeVerificationKeyHash verificationKey
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Verification key hash: " ++ show verificationKeyHash

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Policy: " ++ show script
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Policy ID: " ++ show scriptHash

    liftIO
      $ whenJust scriptFile
        (`LBS.writeFile` encodePretty script)
