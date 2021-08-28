
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Info (
  command
, mainUtxo
, mainAddress
, mainTxBody
, mainTx
) where


import Cardano.Api (AsType(AsTx, AsTxBody), ConsensusModeParams(CardanoModeParams), EpochSlots(..), IsCardanoEra, NetworkId(..), NetworkMagic(..), ShelleyBasedEra, getTxBody, getTxId, readFileTextEnvelope)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Command.Types (Configuration(..), Mantis(InfoAddress, InfoTx, InfoTxBody, InfoUtxo))
import Mantis.Query (queryUTxO)
import Mantis.Transaction (printUTxO)
import Mantis.Types (MantisM, foistMantisEitherIO, printMantis)
import Mantis.Wallet (readAddress)

import qualified Options.Applicative as O


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


mainUtxo :: IsCardanoEra era
         => MonadFail m
         => MonadIO m
         => ShelleyBasedEra era
         -> (String -> MantisM m ())
         -> FilePath
         -> [String]
         -> MantisM m ()
mainUtxo sbe debugMantis configFile addresses =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    printMantis ""
    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantis $ "Network: " ++ show network

    forM_ addresses
      $ \address ->
        do
          debugMantis ""
          address' <- readAddress address
          debugMantis "Output Address: "
          debugMantis $ "  " ++ show address
          debugMantis $ "  " ++ show address'
          printMantis "Unspent UTxO:"
          utxo <- queryUTxO sbe socketPath protocol address' network
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


mainTxBody :: IsCardanoEra era
           => MonadIO m
           => AsType era
           -> (String -> MantisM m ())
           -> [FilePath]
           -> MantisM m ()
mainTxBody asEra _ txBodyFiles =
  forM_ txBodyFiles
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction body file: " ++ file
          txBody <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTxBody asEra) file
          printMantis . show $ getTxId txBody
          printMantis $ show txBody


mainTx :: IsCardanoEra era
       => MonadIO m
       => AsType era
       -> (String -> MantisM m ())
       -> [FilePath]
       -> MantisM m ()
mainTx asEra _ txFiles =
  forM_ txFiles
      $ \file ->
        do
          printMantis ""
          printMantis $ "Transaction file: " ++ file
          tx <-
            foistMantisEitherIO
              $ readFileTextEnvelope (AsTx asEra) file
          printMantis . show . getTxId $ getTxBody tx
          printMantis $ show tx
