
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command (
  main
) where


import Data.Version (Version, showVersion)
import Mantis.Command.Types (Mantis(..))
import Mantis.Types (debugMantis, runMantisToIO)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Mantis.Command.Fingerprint as Fingerprint
import qualified Mantis.Command.Info        as Info
import qualified Mantis.Command.Mint        as Mint
import qualified Mantis.Command.Script      as Script
import qualified Mantis.Command.Transact    as Transact
import qualified Options.Applicative        as O


data Command =
  Command
  {
    quiet  :: Bool
  , mantis :: Mantis
  }
    deriving (Eq, Ord, Read, Show)


main :: Version -> IO ()
main version =
  do
    let
      parser =
        O.info
          (
                O.helper
            <*> versionOption
            <*> (
                      Command
                  <$> verboseOption
                  <*> O.hsubparser (
                           Fingerprint.command
                        <> Info.command
                        <> Mint.command
                        <> Script.command
                        <> Transact.command
                      )
                )
          )
          (
               O.fullDesc
            <> O.progDesc "Utilities for Cardano scripts."
            <> O.header "Mantis Cardano tool."
          )
      versionOption =
        O.infoOption
          ("Mantis " ++ showVersion version)
          (O.long "version" <> O.help "Show version.")
      verboseOption =
        O.switch
          (O.long "quiet" <> O.help "Minimal output.")
    Command{..} <- O.execParser parser
    let
      printer = if quiet then const $ return () else debugMantis
    result <- runMantisToIO
      $ case mantis of
          Transact{..}    -> Transact.main printer configFile tokenName tokenCount tokenSlot outputAddress scriptFile metadataFile
          Mint{..}        -> Mint.main printer configFile mintingFile tokenSlot outputAddress scriptFile metadataFile
          Script{..}      -> Script.main printer configFile tokenSlot scriptFile
          Fingerprint{..} -> Fingerprint.main printer policyId assetName 
          Info{..}        -> Info.main printer configFile outputAddress txBodyFile txFile
    case result of
      Right () -> return ()
      Left e -> hPutStrLn stderr e >> exitFailure
