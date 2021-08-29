
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Chain (
  command
, command'
, main
) where


import Cardano.Api (BlockHeader(..), ConsensusModeParams(CardanoModeParams), EpochSlots(..), NetworkId(..), NetworkMagic(..), serialiseToRawBytesHex)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Mantis.Chain (extractScripts)
import Mantis.Command.Types (Configuration(..), Mantis(..))
import Mantis.Types (MantisM)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS  (unpack)
import qualified Data.ByteString.Lazy  as LBS (writeFile)
import qualified Options.Applicative   as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "watch-scripts"
    $ O.info options (O.progDesc "Download scripts used as transaction witnesses.")


command' :: O.Mod O.CommandFields Mantis
command' =
  O.command "chain-scripts"
    $ O.info options (O.progDesc "[Renamed to 'watch-scripts'.]")


options :: O.Parser Mantis
options =
  Chain
    <$>             O.strArgument   (                                       O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                      )
    <*> O.optional (O.strOption     $                 O.long "output"    <> O.metavar "OUTPUT_DIR"  <> O.help "Output directory for script files."                               )
    <*> O.switch   (                                  O.long "continue"  <>                            O.help "Whether to continue when the current tip of the chain is reached.")


main :: MonadFail m
     => MonadIO m
     => (String -> IO ())
     -> FilePath
     -> Maybe FilePath
     -> Bool
     -> MantisM m ()
main debugIO configFile output continue =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    liftIO $ debugIO ""
    liftIO . debugIO $ "Network: " ++ show network

    extractScripts socketPath protocol network (return $ not continue)
      $ \(BlockHeader slotNo _ _) txId hash script ->
        do
          let
            hash' = BS.unpack $ serialiseToRawBytesHex hash
          debugIO ""
          debugIO $ show slotNo
          debugIO $ show txId
          debugIO $ "Hash " ++ hash'
          debugIO $ show script
          whenJust output
            $ \output' ->
              LBS.writeFile (output' </> (hash' ++ ".json"))
                $ encodePretty script
