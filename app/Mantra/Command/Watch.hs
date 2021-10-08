
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Mantra.Command.Watch (
  command
, mainAddress
, mainCoin
) where


import Cardano.Api (AssetId(..), AsType(AsAssetName, AsPolicyId), BlockHeader(..), BlockNo(..), ChainPoint(..), ChainTip(..), ConsensusModeParams(CardanoModeParams), EpochSlots(..), NetworkId(..), NetworkMagic(..), SlotNo(..), TxOut(..), TxOutValue(..), deserialiseFromRawBytes, deserialiseFromRawBytesHex, selectAsset, valueToList)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantra.Chain (Reverter, loadPoint, savePoint, watchTransactions)
import Mantra.Command.Types (Configuration(..), Mantra(..))
import Mantra.Types (MantraM, foistMantraMaybe)
import Mantra.Transaction (printValueIO)
import Mantra.Wallet (readAddress, showAddressInEra)

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Options.Applicative   as O


command :: O.Mod O.CommandFields Mantra
command =
     O.command "watch-address" (O.info optionsAddress $ O.progDesc "Watch transactions at an address.")
  <> O.command "watch-coin"    (O.info optionsCoin    $ O.progDesc "Watch transactions for a coin."   )


optionsAddress :: O.Parser Mantra
optionsAddress =
  WatchAddress
    <$>             O.strArgument (                      O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                      )
    <*> O.many     (O.strArgument $                      O.metavar "ADDRESS"     <> O.help "Shelley address."                                                 )
    <*> O.switch   (                O.long "continue"                            <> O.help "Whether to continue when the current tip of the chain is reached.")
    <*> O.optional (O.strOption   $ O.long "restart"  <> O.metavar "POINT_FILE"  <> O.help "File for restoring and saving current point on the chain."        )

optionsCoin :: O.Parser Mantra
optionsCoin =
  WatchCoin
    <$>             O.strArgument (                     O.metavar "CONFIG_FILE" <> O.help "Path to configuration file."                                      )
    <*>             O.strArgument (                     O.metavar "POLICY_ID"   <> O.help "Policy ID for the token."                                         )
    <*> O.optional (O.strArgument                     $ O.metavar "ASSET_NAME"  <> O.help "Asset name for the token."                                        )
    <*> O.switch   (                O.long "continue"                           <> O.help "Whether to continue when the current tip of the chain is reached.")
    <*> O.optional (O.strOption   $ O.long "restart"  <> O.metavar "POINT_FILE" <> O.help "File for restoring and saving current point on the chain."        )


mainAddress :: MonadFail m
            => MonadIO m
            => (String -> IO ())
            -> FilePath
            -> [String]
            -> Bool
            -> Maybe FilePath
            -> MantraM m ()
mainAddress debugIO configFile addresses continue pointFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile
    start <- liftIO $ loadPoint pointFile
    mapM_ readAddress addresses

    let
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
      ignoreBlocks = const . const $ return ()
      ignoreTxIns = const . const $ return ()
    liftIO $ debugIO ""
    liftIO . debugIO $ "Network: " ++ show network

    watchTransactions socketPath protocol network start (savePoint pointFile) (Just reportReversion) (return $ not continue) ignoreBlocks ignoreTxIns
      $ \(BlockHeader slotNo _ _) _ txIn (TxOut address txOutValue _) ->
        case txOutValue of
          TxOutValue _ value ->
            when (showAddressInEra address `elem` addresses)
              $ do
                print txIn
                putStrLn $ "  " ++ show slotNo
                putStrLn $ "  " ++ showAddressInEra address
                printValueIO "  " value
          _ -> return ()


mainCoin :: MonadFail m
         => MonadIO m
         => (String -> IO ())
         -> FilePath
         -> String
         -> Maybe String
         -> Bool
         -> Maybe FilePath
         -> MantraM m ()
mainCoin debugIO configFile policyId assetName continue pointFile =
  do
    Configuration{..} <- liftIO $ read <$> readFile configFile
    start <- liftIO $ loadPoint pointFile
    policyId' <-
      foistMantraMaybe "Could not decode policy ID."
        . deserialiseFromRawBytesHex AsPolicyId
        $ BS.pack policyId
    assetFilter <-
      case assetName of
        Just assetName' -> do
                             assetName'' <-
                               foistMantraMaybe "Could not decode asset name."
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
      ignoreBlocks = const . const $ return ()
      ignoreTxIns = const . const $ return ()
    liftIO $ debugIO ""
    liftIO . debugIO $ "Network: " ++ show network

    watchTransactions socketPath protocol network start (savePoint pointFile) (Just reportReversion) (return $ not continue) ignoreBlocks ignoreTxIns
      $ \(BlockHeader slotNo _ _) _ txIn (TxOut address txOutValue _) ->
        case txOutValue of
          TxOutValue _ value ->
            when (assetFilter value)
              $ do
                print txIn
                putStrLn $ "  " ++ show slotNo
                putStrLn $ "  " ++ showAddressInEra address
                printValueIO "  " value
          _ -> return ()


reportReversion :: Reverter
reportReversion point tip =
  do
    let
      pointSlot =
        case point of
          ChainPointAtGenesis -> SlotNo 0
          ChainPoint slot _   -> slot
      (tipSlot, tipBlock) =
        case tip of
          ChainTipAtGenesis     -> (SlotNo 0, BlockNo 0)
          ChainTip slot _ block -> (slot, block)
    putStrLn "Rollback:"
    putStrLn $ "  Point: " ++ show pointSlot
    putStrLn $ "  Tip: " ++ show tipSlot ++ " " ++ show tipBlock
