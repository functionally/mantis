
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command (
  main
) where


import Data.Version (Version, showVersion)
import Mantis.Command.Types (Mantis(..))
import Mantis.Types (runMantisToIO)
import System.IO (hPutStrLn, stderr)

import qualified Mantis.Command.Fingerprint as Fingerprint
import qualified Mantis.Command.Mint        as Mint
import qualified Mantis.Command.Script      as Script
import qualified Mantis.Command.Transact    as Transact
import qualified Options.Applicative        as O


main :: Version -> IO ()
main version =
  do
    let
      parser =
        O.info
          (
                O.helper
            <*> versionOption
            <*> O.hsubparser
                (
                     Fingerprint.command
                  <> Mint.command
                  <> Script.command
                  <> Transact.command
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
    command <- O.execParser parser
    result <- runMantisToIO
      $ case command of
          Transact{..}    -> Transact.main configFile tokenName tokenCount tokenSlot outputAddress scriptFile metadataFile
          Mint{..}        -> Mint.main configFile mintingFile tokenSlot outputAddress scriptFile metadataFile
          Script{..}      -> Script.main configFile tokenSlot scriptFile
          Fingerprint{..} -> Fingerprint.main policyId assetName 
    case result of
      Right () -> return ()
      Left e -> hPutStrLn stderr e
