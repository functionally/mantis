
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Command (
  main
) where


import Data.Version (Version, showVersion)
import Mantis.Command.Types (Mantis(..))
import Mantis.Types (debugMantis, runMantisToIO)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Mantis.Command.Bech32      as Bech32
--import qualified Mantis.Command.Chain       as Chain
import qualified Mantis.Command.Fingerprint as Fingerprint
--import qualified Mantis.Command.Info        as Info
--import qualified Mantis.Command.Mint        as Mint
import qualified Mantis.Command.Script      as Script
--import qualified Mantis.Command.Transact    as Transact
--import qualified Mantis.Command.Watch       as Watch
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
                           Bech32.command
--                        <> Chain.command
                        <> Fingerprint.command
--                      <> Info.command
--                        <> Mint.command
                        <> Script.command
--                        <> Transact.command
--                        <> Watch.command
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
          ("Mantis " ++ showVersion version ++ ", (c) 2021 Brian W Bush <code@functionally.io>")
          (O.long "version" <> O.help "Show version.")
      verboseOption =
        O.switch
          (O.long "quiet" <> O.help "Minimal output.")
    Command{..} <- O.execParser parser
    let
      printer  = if quiet then const $ return () else debugMantis
      printer' = if quiet then const $ return () else hPutStrLn stderr
    result <- runMantisToIO
      $ case mantis of
--        Transact{..}     -> Transact.main printer configFile tokenName tokenCount tokenSlot outputAddress scriptFile metadataFile
--        Mint{..}         -> Mint.main printer configFile mintingFile tokenSlot outputAddress scriptFile metadataFile
          Script{..}       -> Script.main printer configFile tokenSlot scriptFile
          Fingerprint{..}  -> Fingerprint.main printer policyId assetName 
--        InfoUtxo{..}     -> Info.mainUtxo printer configFile addresses
--        InfoAddress{..}  -> Info.mainAddress printer addresses
--        InfoTxBody{..}   -> Info.mainTxBody printer txBodyFiles
--        InfoTx{..}       -> Info.mainTx printer txFiles
          Bech32Decode{..} -> Bech32.mainDecode printer bech32
          Bech32Encode{..} -> Bech32.mainEncode printer humanReadablePart dataPart
--        Chain{..}        -> Chain.main printer' configFile outputDirectory continue
--        WatchAddress{..} -> Watch.mainAddress printer' configFile addresses continue
--        WatchCoin{..}    -> Watch.mainCoin printer' configFile policyId assetName' continue
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
