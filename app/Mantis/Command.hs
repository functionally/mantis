
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command (
  main
) where


import Data.Version (Version, showVersion)
import Mantis.Command.Types (Mantis(..))

import qualified Mantis.Command.Fingerprint as Fingerprint
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
            <*> O.hsubparser (Transact.command <> Fingerprint.command)
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
    case command of
      Transact{..}    -> Transact.main configFile tokenName tokenCount tokenSlot metadataFile
      Fingerprint{..} -> Fingerprint.main policyId assetName 

