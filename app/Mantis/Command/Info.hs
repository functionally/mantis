
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Info (
  command
, mainUtxo
, mainAddress
, mainTxBody
, mainTx
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Typed (AsType(AsTx, AsTxBody, AsMaryEra), NetworkMagic(..), getTxBody, readFileTextEnvelope)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Command.Types (Configuration(..), Mantis(InfoAddress, InfoTx, InfoTxBody, InfoUtxo))
import Mantis.Query (queryUTxO)
import Mantis.Transaction (printUTxO)
import Mantis.Types (MantisM, foistMantisEitherIO, printMantis)
import Mantis.Wallet (readAddress)

import qualified Cardano.Chain.Slotting as Chain (EpochSlots(..))
import qualified Options.Applicative    as O


command :: O.Mod O.CommandFields Mantis
command =
  mconcat
    [
      O.command "info-address" $ O.info
         (
           InfoAddress
             <$> O.many (O.strArgument $ O.metavar "ADDRESS" <> O.help "Shelley address.")
         )
         (O.progDesc "Print information about addresses.")
    , O.command "info-tx" $ O.info
         (
           InfoTx
             <$>  O.many (O.strArgument $ O.metavar "TX_FILE" <> O.help "Signed transaction file.")
         )
         (O.progDesc "Print contents of transaction files.")
    , O.command "info-txbody" $ O.info
         (
           InfoTxBody
             <$> O.many (O.strArgument $ O.metavar "TXBODY_FILE" <> O.help "Transaction body file.")
         )
         (O.progDesc "Print contents of transaction body files.")
    , O.command "info-utxo" $ O.info
        (
          InfoUtxo
            <$>         O.strArgument ( O.metavar "CONFIG_FILE" <> O.help "Path to configuration file.")
            <*> O.many (O.strArgument $ O.metavar "ADDRESS"     <> O.help "Shelley address."           )
        )
        (O.progDesc "Print UTxO information for addresses.")
    ]


mainUtxo :: MonadFail m
         => MonadIO m
         => (String -> MantisM m ())
         -> FilePath
         -> [String]
         -> MantisM m ()
mainUtxo debugMantis configFile addresses =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    printMantis ""
    let
      protocol = CardanoProtocol $ Chain.EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantis $ "Network: " ++ show network

    forM_ addresses
      $ \address ->
        do
          printMantis ""
          address' <- readAddress address
          printMantis "Output Address: "
          printMantis $ "  " ++ show address
          printMantis $ "  " ++ show address'
          printMantis "Unspent UTxO:"
          utxo <- queryUTxO protocol address' network
          printUTxO "  " utxo


mainAddress :: MonadIO m
            => (String -> MantisM m ())
            -> [String]
            -> MantisM m ()
mainAddress _ addresses =
  forM_ addresses
    $ \address ->
      do
        printMantis ""
        address' <- readAddress address
        printMantis "Output Address: "
        printMantis $ "  " ++ show address
        printMantis $ "  " ++ show address'


mainTxBody :: MonadIO m
           => (String -> MantisM m ())
           -> [FilePath]
           -> MantisM m ()
mainTxBody _ txBodyFiles =
  forM_ txBodyFiles
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction body file: " ++ file
          txBody <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTxBody AsMaryEra) file
          printMantis . show $ getTxId txBody
          printMantis $ show txBody


mainTx :: MonadIO m
       => (String -> MantisM m ())
       -> [FilePath]
       -> MantisM m ()
mainTx _ txFiles =
  forM_ txFiles
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction file: " ++ file
          tx <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTx AsMaryEra) file
          printMantis . show . getTxId $ getTxBody tx
          printMantis $ show tx