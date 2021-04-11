
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Script (
  command
, main
) where


import Cardano.Api (NetworkId(..))
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Typed (NetworkMagic(..))
import Control.Monad.Except (runExceptT)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantis.Command.Types (Configuration(..), Mantis(..), SlotRef)
import Mantis.Query (adjustSlot, queryTip)
import Mantis.Script (mintingScript)
import Mantis.Wallet (makeVerificationKeyHash, readVerificationKey)

import qualified Cardano.Chain.Slotting as Chain (EpochSlots(..))
import qualified Data.ByteString.Lazy   as LBS   (writeFile)
import qualified Options.Applicative    as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "script"
    $ O.info options (O.progDesc "Construct a minting script and compute its hash.")


options :: O.Parser Mantis
options =
  Script
    <$>             O.strArgument   (                      O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                                               )
    <*> O.optional (O.strArgument   $                      O.metavar "SCRIPT_FILE" <> O.help "Path to script JSON file."                                                                 )
    <*> O.optional (O.option O.auto $ O.long  "expires" <> O.metavar "SLOT"        <> O.help "Slot number after which tokens are not mintable / burnable; prefix `+` if relative to tip.")


main :: FilePath
     -> Maybe SlotRef
     -> Maybe FilePath
     -> IO ()
main configFile tokenSlot scriptFile =
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

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    putStrLn ""
    putStrLn $ "Verification key hash: " ++ show verificationKeyHash

    let
      (script, scriptHash) = mintingScript verificationKeyHash before
    putStrLn ""
    putStrLn $ "Policy: " ++ show script
    putStrLn ""
    putStrLn $ "Policy ID: " ++ show scriptHash

    maybe (return ()) (`LBS.writeFile` encodePretty script) scriptFile
