
{-# LANGUAGE RecordWildCards   #-}


module Mantra.Command (
  main
) where


import Cardano.Api          (IsShelleyBasedEra, ShelleyBasedEra)
import Data.Version         (Version, showVersion)
import Mantra.Command.Types (Mantra(..))
import Mantra.Types         (debugMantra, runMantraToIO)
import System.Exit          (exitFailure)
import System.IO            (hPutStrLn, stderr)

import qualified Mantra.Command.Bech32      as Bech32
import qualified Mantra.Command.Chain       as Chain
import qualified Mantra.Command.Fingerprint as Fingerprint
import qualified Mantra.Command.Info        as Info
import qualified Mantra.Command.Mint        as Mint
import qualified Mantra.Command.Script      as Script
import qualified Mantra.Command.Transact    as Transact
import qualified Mantra.Command.Watch       as Watch
import qualified Options.Applicative        as O


data Command =
  Command
  {
    quiet  :: Bool
  , mantra :: Mantra
  }
    deriving (Eq, Ord, Read, Show)


main :: IsShelleyBasedEra era
     => Version
     -> ShelleyBasedEra era
     -> IO ()
main version sbe =
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
                        <> Chain.command'
                        <> Fingerprint.command
                        <> Info.command
                        <> Mint.command
                        <> Script.command
                        <> Transact.command
                        <> Watch.command
                        <> Chain.command
                      )
                )
          )
          (
               O.fullDesc
            <> O.progDesc "Utilities for Cardano scripts."
            <> O.header "Mantra Cardano tool."
          )
      versionOption =
        O.infoOption
          ("Mantra " ++ showVersion version ++ ", (c) 2021 Brian W Bush <code@functionally.io>")
          (O.long "version" <> O.help "Show version.")
      verboseOption =
        O.switch
          (O.long "quiet" <> O.help "Minimal output.")
    Command{..} <- O.execParser parser
    let
      printer  = if quiet then const $ return () else debugMantra
      printer' = if quiet then const $ return () else hPutStrLn stderr
    result <- runMantraToIO
      $ case mantra of
          Transact{..}     -> Transact.main sbe printer configFile tokenName tokenCount tokenSlot outputAddress scriptFile metadataFile
          Mint{..}         -> Mint.main sbe printer configFile mintingFile tokenSlot outputAddress scriptFile metadataFile
          Script{..}       -> Script.main printer configFile tokenSlot scriptFile
          Fingerprint{..}  -> Fingerprint.main printer policyId assetName
          InfoUtxo{..}     -> Info.mainUtxo sbe printer configFile addresses
          InfoAddress{..}  -> Info.mainAddress printer addresses
          InfoTxBody{..}   -> Info.mainTxBody printer txBodyFiles
          InfoTx{..}       -> Info.mainTx printer txFiles
          Bech32Decode{..} -> Bech32.mainDecode printer bech32
          Bech32Encode{..} -> Bech32.mainEncode printer humanReadablePart dataPart
          Chain{..}        -> Chain.main printer' configFile outputDirectory continue pointFile
          WatchAddress{..} -> Watch.mainAddress printer' configFile addresses continue pointFile
          WatchCoin{..}    -> Watch.mainCoin printer' configFile policyId assetName' continue pointFile
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
