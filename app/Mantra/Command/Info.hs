
{-# LANGUAGE RecordWildCards #-}


module Mantra.Command.Info (
  command
, mainUtxo
, mainAddress
, mainTxBody
, mainTx
) where


import Cardano.Api            (AsType(AsAllegraEra, AsAlonzoEra, AsByronEra, AsMaryEra, AsShelleyEra, AsTx, AsTxBody), ConsensusModeParams(CardanoModeParams), EpochSlots(..), FromSomeType(..), IsCardanoEra, NetworkId(..), NetworkMagic(..), ShelleyBasedEra, getTxBody, getTxId, readFileTextEnvelopeAnyOf)
import Control.Monad          (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantra.Command.Types   (Configuration(..), Mantra(InfoAddress, InfoTx, InfoTxBody, InfoUtxo))
import Mantra.Query           (queryUTxO)
import Mantra.Transaction     (printUTxO)
import Mantra.Types           (MantraM, foistMantraEitherIO, printMantra)
import Mantra.Wallet          (readAddress)

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


mainTxBody :: MonadIO m
           => (String -> MantraM m ())
           -> [FilePath]
           -> MantraM m ()
mainTxBody _ txBodyFiles =
  forM_ txBodyFiles
      $ \file ->
        do
          printMantra ""
          printMantra $ "Transaction body file: " ++ file
          (txId, txBody) <-
            foistMantraEitherIO
              $ readFileTextEnvelopeAnyOf
                [
                  FromSomeType (AsTxBody AsAlonzoEra ) $ \txBody -> (show $ getTxId txBody, show txBody)
                , FromSomeType (AsTxBody AsMaryEra   ) $ \txBody -> (show $ getTxId txBody, show txBody)
                , FromSomeType (AsTxBody AsAllegraEra) $ \txBody -> (show $ getTxId txBody, show txBody)
                , FromSomeType (AsTxBody AsShelleyEra) $ \txBody -> (show $ getTxId txBody, show txBody)
                , FromSomeType (AsTxBody AsByronEra  ) $ \txBody -> (show $ getTxId txBody, show txBody)
                ]
                file
          printMantra txId
          printMantra txBody


mainTx :: MonadIO m
       => (String -> MantraM m ())
       -> [FilePath]
       -> MantraM m ()
mainTx _ txFiles =
  forM_ txFiles
      $ \file ->
        do
          printMantra ""
          printMantra $ "Transaction file: " ++ file
          (txId, tx) <-
            foistMantraEitherIO
              $ readFileTextEnvelopeAnyOf
                [
                  FromSomeType (AsTx AsAlonzoEra ) $ \tx -> (show . getTxId $ getTxBody tx, show tx)
                , FromSomeType (AsTx AsMaryEra   ) $ \tx -> (show . getTxId $ getTxBody tx, show tx)
                , FromSomeType (AsTx AsAllegraEra) $ \tx -> (show . getTxId $ getTxBody tx, show tx)
                , FromSomeType (AsTx AsShelleyEra) $ \tx -> (show . getTxId $ getTxBody tx, show tx)
                , FromSomeType (AsTx AsByronEra  ) $ \tx -> (show . getTxId $ getTxBody tx, show tx)
                ]
                file
          printMantra txId
          printMantra tx
