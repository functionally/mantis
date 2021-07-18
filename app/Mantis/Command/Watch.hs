
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Mantis.Command.Watch (
  command
, mainAddress
, mainCoin
) where


import Cardano.Api (AssetId(..), AsType(AsAssetName, AsPolicyId), BlockHeader(..), ConsensusModeParams(CardanoModeParams), EpochSlots(..), NetworkId(..), NetworkMagic(..), TxOut(..), TxOutValue(..), anyAddressInShelleyBasedEra, deserialiseFromRawBytes, deserialiseFromRawBytesHex, selectAsset, valueToList)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Chain (Reverter, watchTransactions)
import Mantis.Command.Types (Configuration(..), Mantis(..))
import Mantis.Types (MantisM, foistMantisMaybe)
import Mantis.Transaction (printValueIO)
import Mantis.Wallet (readAddress, showAddressMary)

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Options.Applicative   as O


command :: O.Mod O.CommandFields Mantis
command =
     O.command "watch-address" (O.info optionsAddress $ O.progDesc "Watch transactions at an address.")
  <> O.command "watch-coin"    (O.info optionsCoin    $ O.progDesc "Watch transactions for a coin."   )


optionsAddress :: O.Parser Mantis
optionsAddress =
  WatchAddress
    <$>                  O.strArgument (                   O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                      )
    <*> O.many          (O.strArgument $                   O.metavar "ADDRESS"     <> O.help "Shelley address."                                                 )
    <*> O.switch        (                O.long "continue"                         <> O.help "Whether to continue when the current tip of the chain is reached.")

optionsCoin :: O.Parser Mantis
optionsCoin =
  WatchCoin
    <$>                  O.strArgument (                     O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                      )
    <*>                  O.strArgument (                     O.metavar "POLICY_ID"   <> O.help "Policy ID for the token."                                         )
    <*> O.optional      (O.strArgument                     $ O.metavar "ASSET_NAME"  <> O.help "Asset name for the token."                                        )
    <*> O.switch        (                O.long "continue"                           <> O.help "Whether to continue when the current tip of the chain is reached.")


mainAddress :: MonadFail m
            => MonadIO m
            => (String -> IO ())
            -> FilePath
            -> [String]
            -> Bool
            -> MantisM m ()
mainAddress debugIO configFile addresses continue =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile
    addresses' <- mapM (fmap anyAddressInShelleyBasedEra . readAddress) addresses

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
      ignoreTxIns = const . const $ return ()
    liftIO $ debugIO ""
    liftIO . debugIO $ "Network: " ++ show network

    watchTransactions socketPath protocol network (Just reportReversion) (return $ not continue) ignoreTxIns
      $ \(BlockHeader slotNo _ _) _ txIn (TxOut address txOutValue) ->
        case txOutValue of
          TxOutValue _ value -> 
            when (address `elem` addresses')
              $ do
                print txIn
                putStrLn $ "  " ++ show slotNo
                putStrLn $ "  " ++ showAddressMary address
                printValueIO "  " value
          _ -> return ()


mainCoin :: MonadFail m
         => MonadIO m
         => (String -> IO ())
         -> FilePath
         -> String
         -> Maybe String
         -> Bool
         -> MantisM m ()
mainCoin debugIO configFile policyId assetName continue =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile
    policyId' <-
      foistMantisMaybe "Could not decode policy ID."
        . deserialiseFromRawBytesHex AsPolicyId
        $ BS.pack policyId
    assetFilter <-
      case assetName of
        Just assetName' -> do
                             assetName'' <-
                               foistMantisMaybe "Could not decode asset name."
                                 . deserialiseFromRawBytes AsAssetName
                                 $ BS.pack assetName'
                             return
                               $ \value ->
                                 selectAsset value (AssetId policyId' assetName'') > 0
        Nothing         -> return
                             $ \value ->
                                any
                                  (
                                    \case
                                      (AssetId policyId'' _, _) -> policyId' == policyId''
                                      _                         -> False
                                  )
                                  $ valueToList value

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
      ignoreTxIns = const . const $ return ()
    liftIO $ debugIO ""
    liftIO . debugIO $ "Network: " ++ show network

    watchTransactions socketPath protocol network (Just reportReversion) (return $ not continue) ignoreTxIns
      $ \(BlockHeader slotNo _ _) _ txIn (TxOut address txOutValue) ->
        case txOutValue of
          TxOutValue _ value -> 
            when (assetFilter value)
              $ do
                print txIn
                putStrLn $ "  " ++ show slotNo
                putStrLn $ "  " ++ showAddressMary address
                printValueIO "  " value
          _ -> return ()


reportReversion :: Reverter
reportReversion point tip =
  do
    putStrLn "Rollback:"
    putStrLn $ "  " ++ show point
    putStrLn $ "  " ++ show tip
