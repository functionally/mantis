
{-# LANGUAGE RecordWildCards #-}


module Mantra.Command.Info (
  command
, mainUtxo
, mainAddress
, mainTxBody
, mainTx
) where


import Cardano.Api (AsType(AsTx, AsTxBody), ConsensusModeParams(CardanoModeParams), EpochSlots(..), IsCardanoEra, NetworkId(..), NetworkMagic(..), ShelleyBasedEra, getTxBody, getTxId, readFileTextEnvelope)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantra.Command.Types (Configuration(..), Mantra(InfoAddress, InfoTx, InfoTxBody, InfoUtxo))
import Mantra.Query (queryUTxO)
import Mantra.Transaction (printUTxO)
import Mantra.Types (MantraM, foistMantraEitherIO, printMantra)
import Mantra.Wallet (readAddress)

import qualified Options.Applicative as O


command :: O.Mod O.CommandFields Mantra
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
         -> (String -> MantraM m ())
         -> FilePath
         -> [String]
         -> MantraM m ()
mainUtxo sbe debugMantra configFile addresses =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile

    printMantra ""
    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    debugMantra $ "Network: " ++ show network

    forM_ addresses
      $ \address ->
        do
          debugMantra ""
          address' <- readAddress address
          debugMantra "Output Address: "
          debugMantra $ "  " ++ show address
          debugMantra $ "  " ++ show address'
          printMantra "Unspent UTxO:"
          utxo <- queryUTxO sbe socketPath protocol address' network
          printUTxO "  " utxo


mainAddress :: MonadIO m
            => (String -> MantraM m ())
            -> [String]
            -> MantraM m ()
mainAddress _ addresses =
  forM_ addresses
    $ \address ->
      do
        printMantra ""
        address' <- readAddress address
        printMantra "Output Address: "
        printMantra $ "  " ++ show address
        printMantra $ "  " ++ show address'


mainTxBody :: IsCardanoEra era
           => MonadIO m
           => AsType era
           -> (String -> MantraM m ())
           -> [FilePath]
           -> MantraM m ()
mainTxBody asEra _ txBodyFiles =
  forM_ txBodyFiles
      $ \file ->
        do
          printMantra ""
          printMantra $ "Transaction body file: " ++ file
          txBody <-
            foistMantraEitherIO
              $ readFileTextEnvelope (AsTxBody asEra) file
          printMantra . show $ getTxId txBody
          printMantra $ show txBody


mainTx :: IsCardanoEra era
       => MonadIO m
       => AsType era
       -> (String -> MantraM m ())
       -> [FilePath]
       -> MantraM m ()
mainTx asEra _ txFiles =
  forM_ txFiles
      $ \file ->
        do
          printMantra ""
          printMantra $ "Transaction file: " ++ file
          tx <-
            foistMantraEitherIO
              $ readFileTextEnvelope (AsTx asEra) file
          printMantra . show . getTxId $ getTxBody tx
          printMantra $ show tx
