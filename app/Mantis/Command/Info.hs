
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Info (
  command
, main
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Typed (AsType(AsTx, AsTxBody, AsMaryEra), NetworkMagic(..), getTxBody, readFileTextEnvelope)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Command.Types (Configuration(..), Mantis(Info))
import Mantis.Query (queryUTxO)
import Mantis.Transaction (printUTxO)
import Mantis.Types (MantisM, foistMantisEitherIO, printMantis)
import Mantis.Wallet (readAddress)

import qualified Cardano.Chain.Slotting as Chain (EpochSlots(..))
import qualified Options.Applicative    as O


command :: O.Mod O.CommandFields Mantis
command =
  O.command "info"
    $ O.info options (O.progDesc "Print information about a transaction or address.")


options :: O.Parser Mantis
options =
  Info
    <$>             O.strArgument (                     O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."        )
    <*> O.optional (O.strOption   $ O.long "output"  <> O.metavar "ADDRESS"     <> O.help "Address for output of transactions.")
    <*> O.optional (O.strOption   $ O.long "tx-body" <> O.metavar "TXBODY_FILE" <> O.help "Transaction body file."             )
    <*> O.optional (O.strOption   $ O.long "tx"      <> O.metavar "TX_FILE"     <> O.help "Signed transaction file."           )


main :: MonadFail m
     => MonadIO m
     => (String -> MantisM m ())
     -> FilePath
     -> Maybe String
     -> Maybe FilePath
     -> Maybe FilePath
     -> MantisM m ()
main debugMantis configFile outputAddress txBodyFile txFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    whenJust outputAddress
      $ \address ->
        do
          printMantis ""
          let
            protocol = CardanoProtocol $ Chain.EpochSlots epochSlots
            network = maybe Mainnet (Testnet . NetworkMagic) magic
          debugMantis $ "Network: " ++ show network
          address' <- readAddress address
          printMantis "Output Address: "
          printMantis $ "  " ++ show address
          printMantis $ "  " ++ show address'
          printMantis "Unspent UTxO:"
          utxo <- queryUTxO protocol address' network
          printUTxO "  " utxo

    whenJust txBodyFile
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction body file: " ++ file
          txBody <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTxBody AsMaryEra) file
          printMantis . show $ getTxId txBody
          printMantis $ show txBody

    whenJust txFile
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction file: " ++ file
          tx <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTx AsMaryEra) file
          printMantis . show . getTxId $ getTxBody tx
          printMantis $ show tx
