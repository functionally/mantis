-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Watching activity on the blockchain.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Mantra.Chain (
-- * Handlers
  Recorder
, Processor
, Reverter
, IdleNotifier
, ScriptHandler
, PlutusHandler
, TxInHandler
, TxOutHandler
-- * Activity
, walkBlocks
, extractScripts
, extractPlutus
, watchTransactions
-- * Points
, savePoint
, loadPoint
) where


import Cardano.Api (AsType(AsHash, AsBlockHeader), Block(..), BlockHeader(..), BlockInMode(..), CardanoMode, ChainPoint(..), ChainTip, ConsensusModeParams, EraInMode(..), IsCardanoEra, LocalNodeClientProtocols(..), LocalChainSyncClient(..), LocalNodeConnectInfo(..), NetworkId, ScriptHash, ShelleyBasedEra(..), SimpleScript, SimpleScriptV2, SlotNo(..), TxBody(..), TxBodyContent(..), TxId, TxIn(..), TxIx(..), TxOut(..), connectToLocalNode, deserialiseFromRawBytesHex, getTxBody, getTxId, serialiseToRawBytesHex)
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStIntersect(..), ClientStNext(..))
import Cardano.Api.Shelley          (PlutusScript(..), PlutusScriptV1, TxBody(ShelleyTxBody))
import Control.Monad.Extra          (whenJust)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.Maybe                   (catMaybes, fromMaybe)
import Data.Word                    (Word64)
import Mantra.Chain.Internal        (interpretAsScript)
import Mantra.Types                 (MantraM)
import System.Directory             (doesFileExist, renameFile)
import Text.Read                    (readMaybe)

import qualified Data.ByteString.Char8              as BS           (pack, unpack)
import qualified Cardano.Ledger.Crypto              as Ledger       (StandardCrypto)
import qualified Cardano.Ledger.Alonzo.Scripts      as LedgerAlonzo (Script(..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA    (Timelock)


-- | A point on the chain.
data SavedPoint =
  SavedPoint
  {
    slotNo    :: Word64 -- ^ The slot number.
  , blockHash :: String -- ^ The block hash.
  }
    deriving (Eq, Ord, Read, Show)


-- | Record the point on the chain.
type Recorder =  BlockInMode CardanoMode -- ^ The block.
              -> IO ()                   -- ^ Action to record the point.


-- | Record the point on the chain into a file.
savePoint :: Maybe FilePath   -- ^ The file in which to record the point, if any.
          -> BlockInMode mode -- ^ The block.
          -> IO ()            -- ^ Action to record the point in the file.
savePoint (Just filename) (BlockInMode (Block (BlockHeader (SlotNo slotNo) blockHash' _) _) _) =
  do
    let
      filename' = filename ++ ".tmp"
    writeFile filename'
      . show
      . SavedPoint slotNo
      $ BS.unpack
      $ serialiseToRawBytesHex blockHash'
    renameFile filename' filename
savePoint Nothing _ = return ()


-- | Set the point on the chain.
loadPoint :: Maybe FilePath -- ^ The file in which the point is recorded, if any.
          -> IO ChainPoint  -- ^ Action to read the point, if possible.
loadPoint (Just filename) =
  do
    exists <- doesFileExist filename
    point <-
      if exists
        then readMaybe <$> readFile filename
        else return Nothing
    return
      . fromMaybe ChainPointAtGenesis
      $ do
        SavedPoint{..} <- point
        blockHash' <- deserialiseFromRawBytesHex (AsHash AsBlockHeader) $ BS.pack blockHash
        return
          $ ChainPoint (SlotNo slotNo) blockHash'
loadPoint Nothing = return ChainPointAtGenesis


-- | Process a block.
type Processor =  BlockInMode CardanoMode -- ^ The block.
               -> ChainTip                -- ^ The chain tip.
               -> IO ()                   -- ^ Action to process activity.


-- | Handle a rollback.
type Reverter =  ChainPoint -- ^ The new chain point.
              -> ChainTip   -- ^ The chain tip.
              -> IO ()      -- ^ Action to handle the rollback.


-- | Peform action when idle.
type IdleNotifier = IO Bool -- ^ Action that returns whether processing should terminate.


-- | Process activity on the blockchain.
walkBlocks :: MonadIO m
           => FilePath                        -- ^ The path to the node's socket.
           -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
           -> NetworkId                       -- ^ The network.
           -> ChainPoint                      -- ^ The starting point.
           -> Recorder                        -- ^ Handle chain points.
           -> IdleNotifier                    -- ^ Handle idleness.
           -> Maybe Reverter                  -- ^ Handle rollbacks.
           -> Processor                       -- ^ Handle blocks.
           -> MantraM m ()                    -- ^ Action to walk the blockchain.
walkBlocks socketPath mode network start record notifyIdle revertPoint processBlock =
  let
    localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    protocols =
      LocalNodeClientProtocols
      {
        localChainSyncClient    = LocalChainSyncClient $ client start record notifyIdle revertPoint processBlock
      , localTxSubmissionClient = Nothing
      , localStateQueryClient   = Nothing
      }
  in
    liftIO
      $ connectToLocalNode localNodeConnInfo protocols


-- | Chain synchronization client.
client :: ChainPoint                                                          -- ^ Starting point.
       -> Recorder                                                            -- ^ Handle chain points.
       -> IdleNotifier                                                        -- ^ Handle idleness.
       -> Maybe Reverter                                                      -- ^ Handle rollbacks.
       -> Processor                                                           -- ^ Handle blocks.
       -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO () -- ^ The chain synchronization client.
client start record notifyIdle revertPoint processBlock =
  ChainSyncClient
    $ let
        clientStart =
          return
            .  SendMsgFindIntersect [start]
            $ ClientStIntersect
              {
                recvMsgIntersectFound    = \ _ _ -> ChainSyncClient clientStIdle
              , recvMsgIntersectNotFound = \_    -> ChainSyncClient clientStIdle
              }
        clientStIdle =
          return
            . SendMsgRequestNext clientStNext
            $ do
              terminate <- notifyIdle
              if terminate
                then clientDone
                else return clientStNext
        clientStNext =
          ClientStNext
          {
            recvMsgRollForward  = \block tip -> ChainSyncClient $ record block
                                                                >> processBlock block tip
                                                                >> clientStIdle
          , recvMsgRollBackward = \point tip -> ChainSyncClient $  whenJust revertPoint (\f -> f point tip)
                                                                >> clientStIdle
          }
        clientDone =
          return
            $ ClientStNext
              {
                recvMsgRollForward  = \_ _ -> ChainSyncClient . pure $ SendMsgDone ()
              , recvMsgRollBackward = \_ _ -> ChainSyncClient . pure $ SendMsgDone ()
              }
     in
      clientStart


-- | Process a simple script.
type ScriptHandler =  BlockHeader                 -- ^ The block header.
                   -> TxId                        -- ^ The transaction identifier.
                   -> ScriptHash                  -- ^ The script's hash.
                   -> SimpleScript SimpleScriptV2 -- ^ The script.
                   -> IO ()                       -- ^ Action to process the script.


-- | Extract simple scripts from the blockchain.
extractScripts :: MonadIO m
               => FilePath                        -- ^ Path to the node's socket.
               -> ConsensusModeParams CardanoMode -- ^ Consensus mode.
               -> NetworkId                       -- ^ The network.
               -> ChainPoint                      -- ^ The starting point.
               -> Recorder                        -- ^ Handle chain points.
               -> IdleNotifier                    -- ^ Handle idleness.
               -> ScriptHandler                   -- ^ Handle a script.
               -> MantraM m ()                    -- ^ Action to extract scripts.
extractScripts socketPath mode network start record notifyIdle handler =
  walkBlocks socketPath mode network start record notifyIdle Nothing
    $ processScripts handler


-- | Process simple scripts.
processScripts :: ScriptHandler           -- ^ Handle a script.
               -> BlockInMode CardanoMode -- ^ The block.
               -> ChainTip                -- ^ The chain tip.
               -> IO ()                   -- ^ Action to process script.
processScripts handler (BlockInMode (Block header txs) _) _ =
  sequence_
    [
      handler header txId hash script
    |
      tx <- txs
    , let body = getTxBody tx
    , let txId = getTxId body
    , witness <- extractTimelocks body
    , let (script, hash) = interpretAsScript witness
    ]


-- | Extract the simple scripts from a transaction body.
extractTimelocks :: TxBody era                                 -- ^ The transaction body.
                 -> [ShelleyMA.Timelock Ledger.StandardCrypto] -- ^ The time-lock scripts.
extractTimelocks (ShelleyTxBody ShelleyBasedEraAllegra _ witnesses _ _ _) = witnesses
extractTimelocks (ShelleyTxBody ShelleyBasedEraMary    _ witnesses _ _ _) = witnesses
extractTimelocks (ShelleyTxBody ShelleyBasedEraAlonzo  _ witnesses _ _ _) =
  catMaybes
    [
     case witness of
       LedgerAlonzo.TimelockScript witness' -> Just witness'
       _                                    -> Nothing
    |
      witness <- witnesses
    ]
extractTimelocks _ = []


-- | Process a Plutus script.
type PlutusHandler =  BlockHeader                 -- ^ The block header.
                   -> TxId                        -- ^ The transaction identifier.
                   -> ScriptHash                  -- ^ The script's hash.
                   -> PlutusScript PlutusScriptV1 -- ^ The script.
                   -> IO ()                       -- ^ Action to process the script.


-- | Extract Plutus scripts from the blockchain.
extractPlutus :: MonadIO m
              => FilePath                        -- ^ Path to the node's socket.
              -> ConsensusModeParams CardanoMode -- ^ Consensus mode.
              -> NetworkId                       -- ^ The network.
              -> ChainPoint                      -- ^ The starting point.
              -> Recorder                        -- ^ Handle chain points.
              -> IdleNotifier                    -- ^ Handle idleness.
              -> PlutusHandler                   -- ^ Handle a script.
              -> MantraM m ()                    -- ^ Action to extract scripts.
extractPlutus socketPath mode network start record notifyIdle handler =
  walkBlocks socketPath mode network start record notifyIdle Nothing
    $ processPlutus handler


-- | Process Plutus scripts.
processPlutus :: PlutusHandler           -- ^ Handle a script.
              -> BlockInMode CardanoMode -- ^ The block.
              -> ChainTip                -- ^ The chain tip.
              -> IO ()                   -- ^ Action to process script.
processPlutus handler (BlockInMode (Block header txs) _) _ =
  sequence_
    [
      let
        hash = undefined -- FIXME: Compute hash!
      in
        handler header txId hash script
    |
      tx <- txs
    , let body = getTxBody tx
    , let txId = getTxId body
    , script <- extractPlutuses body
    ]


-- | Extract the Plutus scripts from a transaction body.
extractPlutuses :: TxBody era                                 -- ^ The transaction body.
                -> [PlutusScript PlutusScriptV1] -- ^ The plutus scripts.
extractPlutuses (ShelleyTxBody ShelleyBasedEraAlonzo  _ witnesses _ _ _) =
  catMaybes
    [
     case witness of
       LedgerAlonzo.PlutusScript witness' -> Just $ PlutusScriptSerialised witness'
       _                                  -> Nothing
    |
      witness <- witnesses
    ]
extractPlutuses _ = []


-- | Process a block.
type BlockHandler =  BlockHeader -- ^ The block header.
                  -> ChainTip    -- ^ The chain tip.
                  -> IO ()       -- ^ Action to process the block.


-- | Process a spent UTxO.
type TxInHandler =  BlockHeader -- ^ The block header.
                 -> TxIn        -- ^ The UTxO.
                 -> IO ()       -- ^ Action to process a spent UTxO.


-- | Process a transaction's output.
type TxOutHandler =  forall era
                  .  IsCardanoEra era
                  => BlockHeader   -- ^ The block header.
                  -> [TxIn]        -- ^ The spent UTxOs.
                  -> TxIn          -- ^ The output UTxO.
                  -> TxOut era     -- ^ The transaction output.
                  -> IO ()         -- ^ Action to process a transaction's output.


-- | Watch transactions on the blockchain. Note that transaction output is reported *before* spent UTxOs.
watchTransactions :: MonadIO m
                  => FilePath                        -- ^ Path to the node's socket.
                  -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
                  -> NetworkId                       -- ^ The network.
                  -> ChainPoint                      -- ^ The starting point.
                  -> Recorder                        -- ^ Handle chain points.
                  -> Maybe Reverter                  -- ^ Handle rollbacks.
                  -> IdleNotifier                    -- ^ Handle idleness.
                  -> BlockHandler                    -- ^ Handle blocks.
                  -> TxInHandler                     -- ^ Handle spent UTxOs.
                  -> TxOutHandler                    -- ^ Handle transaction output.
                  -> MantraM m ()                    -- ^ Action to watch transactions.
watchTransactions socketPath mode network start record revertPoint notifyIdle blockHandler inHandler outHandler =
  walkBlocks socketPath mode network start record notifyIdle revertPoint
    $ processTransactions blockHandler inHandler outHandler


-- | Process transactions.
processTransactions :: BlockHandler            -- ^ Handle blocks.
                    -> TxInHandler             -- ^ Handle spent UTxOs.
                    -> TxOutHandler            -- ^ Handle transaction output.
                    -> BlockInMode CardanoMode -- ^ The block.
                    -> ChainTip                -- ^ The chain tip.
                    -> IO ()                   -- ^ Action to process transactions.
processTransactions blockHandler inHandler outHandler (BlockInMode block AlonzoEraInCardanoMode ) =
  processTransactions' blockHandler inHandler outHandler block
processTransactions blockHandler inHandler outHandler (BlockInMode block MaryEraInCardanoMode   ) =
  processTransactions' blockHandler inHandler outHandler block
processTransactions blockHandler inHandler outHandler (BlockInMode block AllegraEraInCardanoMode) =
  processTransactions' blockHandler inHandler outHandler block
processTransactions blockHandler inHandler outHandler (BlockInMode block ShelleyEraInCardanoMode) =
  processTransactions' blockHandler inHandler outHandler block
processTransactions blockHandler inHandler outHandler (BlockInMode block ByronEraInCardanoMode  ) =
  processTransactions' blockHandler inHandler outHandler block


-- | Process transactions in a Cardano era.
processTransactions' :: IsCardanoEra era
                     => BlockHandler -- ^ Handle blocks.
                     -> TxInHandler  -- ^ Handle spent UTxOs.
                     -> TxOutHandler -- ^ Handle transaction output.
                     -> Block era    -- ^ The block.
                     -> ChainTip     -- ^ The chain tip.
                     -> IO ()        -- ^ Action to process transactions.
processTransactions' blockHandler inHandler outHandler (Block header txs) tip =
  do
    blockHandler header tip
    sequence_
      [
        do
          sequence_
            [
              outHandler
                header
                txins
                (TxIn (getTxId body) (TxIx ix))
                txout
            |
              (ix, txout) <- zip [0..] txouts
            ]
          sequence_
            [
              inHandler header txin
            |
              txin <- txins
            ]
      |
        tx <- txs
      , let body = getTxBody tx
            TxBody content = body
            txins  = fst <$> txIns content
            txouts = txOuts content
      ]
