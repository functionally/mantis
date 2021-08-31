
{-# LANGUAGE RecordWildCards #-}


module Mantra.Command.Script (
  command
, main
) where


import Cardano.Api (ConsensusModeParams(CardanoModeParams), EpochSlots(..), NetworkId(..), NetworkMagic(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantra.Command.Types (Configuration(..), Mantra(..))
import Mantra.Query (adjustSlot, queryTip)
import Mantra.Script (mintingScript)
import Mantra.Types (MantraM, SlotRef)
import Mantra.Wallet (makeVerificationKeyHash, readVerificationKey)

import qualified Data.ByteString.Lazy as LBS   (writeFile)
import qualified Options.Applicative  as O


command :: O.Mod O.CommandFields Mantra
command =
  O.command "script"
    $ O.info options (O.progDesc "Construct a minting script and compute its Policy ID.")


options :: O.Parser Mantra
options =
  Script
    <$>             O.strArgument   (                     O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                                               )
    <*> O.optional (O.option O.auto $ O.long "expires" <> O.metavar "SLOT"        <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")
    <*> O.optional (O.strOption     $ O.long "script"  <> O.metavar "SCRIPT_FILE" <> O.help "Path to output script JSON file."                                                          )


main :: MonadFail m
     => MonadIO m
     => (String -> MantraM m ())
     -> FilePath
     -> Maybe SlotRef
     -> Maybe FilePath
     -> MantraM m ()
main debugMantra configFile tokenSlot scriptFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantra ""
    debugMantra $ "Network: " ++ show network

    tip <- queryTip socketPath protocol network
    debugMantra ""
    debugMantra $ "Tip: " ++ show tip
    let
      before = (`adjustSlot` tip) <$> tokenSlot

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    debugMantra ""
    debugMantra $ "Verification key hash: " ++ show verificationKeyHash

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    debugMantra ""
    debugMantra $ "Policy: " ++ show script
    debugMantra ""
    debugMantra $ "Policy ID: " ++ show scriptHash

    liftIO
      $ whenJust scriptFile
        (`LBS.writeFile` encodePretty script)
