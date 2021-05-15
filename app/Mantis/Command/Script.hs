
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Script (
  command
, main
) where


import Cardano.Api (ConsensusModeParams(CardanoModeParams), EpochSlots(..), NetworkId(..), NetworkMagic(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryTip)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM)
import Mantis.Wallet (makeVerificationKeyHash, readVerificationKey)

import qualified Data.ByteString.Lazy as LBS   (writeFile)
import qualified Options.Applicative  as O


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
     => (String -> MantisM m ())
     -> FilePath
     -> Maybe SlotRef
     -> Maybe FilePath
     -> MantisM m ()
main debugMantis configFile tokenSlot scriptFile =
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

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    debugMantis ""
    debugMantis $ "Verification key hash: " ++ show verificationKeyHash

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    debugMantis ""
    debugMantis $ "Policy: " ++ show script
    debugMantis ""
    debugMantis $ "Policy ID: " ++ show scriptHash

    liftIO
      $ whenJust scriptFile
        (`LBS.writeFile` encodePretty script)
