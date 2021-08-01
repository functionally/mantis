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


{-# LANGUAGE GADTs #-}


module Mantis.Chain (
-- * Handlers
  Processor
, Reverter
, IdleNotifier
, ScriptHandler
, TxInHandler
, TxOutHandler
-- * Activity
, walkBlocks
, extractScripts
, watchTransactions
) where


import Cardano.Api (Block(..), BlockHeader(..), BlockInMode(..), CardanoMode, ChainPoint, ChainTip, ConsensusModeParams, EraInMode(MaryEraInCardanoMode), LocalNodeClientProtocols(..), LocalChainSyncClient(..), LocalNodeConnectInfo(..), MaryEra, NetworkId, Script, ScriptHash, ShelleyBasedEra(..), SimpleScriptV2, Tx, TxIn(..), TxIx(..), TxOut(..), TxOutValue(..), connectToLocalNode, getTxBody, getTxId, getTxWitnesses)
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..))
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromMaryValue, fromShelleyAddr, fromShelleyTxIn)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (toList)
import Mantis.Chain.Internal (interpretAsScript)
import Mantis.Transaction (supportedMultiAsset)
import Mantis.Types (MantisM)

import qualified Cardano.Ledger.ShelleyMA.TxBody as LedgerMA    (TxBody(..))
import qualified Shelley.Spec.Ledger.TxBody      as ShelleySpec (TxOut(..))


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
           -> IdleNotifier                    -- ^ Handle idleness.
           -> Maybe Reverter                  -- ^ Handle rollbacks.
           -> Processor                       -- ^ Handle blocks.
           -> MantisM m ()                    -- ^ Action to walk the blockchain.
walkBlocks socketPath mode network notifyIdle revertPoint processBlock =
  let
    localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    protocols =
      LocalNodeClientProtocols
      {
        localChainSyncClient    = LocalChainSyncClient $ client notifyIdle revertPoint processBlock
      , localTxSubmissionClient = Nothing
      , localStateQueryClient   = Nothing
      }
  in
    liftIO
      $ connectToLocalNode localNodeConnInfo protocols


-- | Chain synchronization client.
client :: IdleNotifier                                                        -- ^ Handle idleness.
       -> Maybe Reverter                                                      -- ^ Handle rollbacks.
       -> Processor                                                           -- ^ Handle blocks.
       -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO () -- ^ The chain synchronization client.
client notifyIdle revertPoint processBlock =
  ChainSyncClient
    $ let
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
            recvMsgRollForward  = \block tip -> ChainSyncClient $ processBlock block tip
                                                                >> clientStIdle
          , recvMsgRollBackward = \point tip -> ChainSyncClient $ whenJust revertPoint (\f -> f point tip)
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
      clientStIdle


-- | Process a script.
type ScriptHandler =  BlockHeader           -- ^ The block header.
                   -> Tx MaryEra            -- ^ The transaction.
                   -> ScriptHash            -- ^ The script's hash.
                   -> Script SimpleScriptV2 -- ^ The script.
                   -> IO ()                 -- ^ Action to process the script.


-- | Extract scripts from the blockchain.
extractScripts :: MonadIO m
               => FilePath                        -- ^ Path to the node's socket.
               -> ConsensusModeParams CardanoMode -- ^ Consensus mode.
               -> NetworkId                       -- ^ The network.
               -> IdleNotifier                    -- ^ Handle idleness.
               -> ScriptHandler                   -- ^ Handle a script.
               -> MantisM m ()                    -- ^ Action to extract scripts.
extractScripts socketPath mode network notifyIdle handler =
  walkBlocks socketPath mode network notifyIdle Nothing
    $ processScripts handler


-- | Process scripts.
processScripts :: ScriptHandler           -- ^ Handle a script.
               -> BlockInMode CardanoMode -- ^ The block.
               -> ChainTip                -- ^ The chain tip.
               -> IO ()                   -- ^ Action to process script.
processScripts handler (BlockInMode (Block header txs) MaryEraInCardanoMode) _tip =
  sequence_
    [
      whenJust (interpretAsScript witness)
        $ \(script, hash) -> handler header tx hash script
    |
      tx <- txs
    , witness <- getTxWitnesses tx
    ]
processScripts _ _ _ = return ()


-- | Process a block.
type BlockHandler =  BlockHeader -- ^ The block header.
                  -> ChainTip    -- ^ The chain tip.
                  -> IO ()       -- ^ Action to process the block.


-- | Process a spent UTxO.
type TxInHandler =  BlockHeader -- ^ The block header.
                 -> TxIn        -- ^ The UTxO.
                 -> IO ()       -- ^ Action to process a spent UTxO.


-- | Process a transaction's output.
type TxOutHandler =  BlockHeader   -- ^ The block header.
                  -> [TxIn]        -- ^ The spent UTxOs.
                  -> TxIn          -- ^ The output UTxO.
                  -> TxOut MaryEra -- ^ The transaction output.
                  -> IO ()         -- ^ Action to process a transaction's output.


-- | Watch transactions on the blockchain. Note that transaction output is reported *before* spent UTxOs.
watchTransactions :: MonadIO m
                  => FilePath                        -- ^ Path to the node's socket.
                  -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
                  -> NetworkId                       -- ^ The network.
                  -> Maybe Reverter                  -- ^ Handle rollbacks.
                  -> IdleNotifier                    -- ^ Handle idleness.
                  -> BlockHandler                    -- ^ Handle blocks.
                  -> TxInHandler                     -- ^ Handle spent UTxOs.
                  -> TxOutHandler                    -- ^ Handle transaction output.
                  -> MantisM m ()                    -- ^ Action to watch transactions.
watchTransactions socketPath mode network revertPoint notifyIdle blockHandler inHandler outHandler =
  walkBlocks socketPath mode network notifyIdle revertPoint
    $ processTransactions blockHandler inHandler outHandler


-- | Process transactions.
processTransactions :: BlockHandler            -- ^ Handle blocks.
                    -> TxInHandler             -- ^ Handle spent UTxOs.
                    -> TxOutHandler            -- ^ Handle transaction output.
                    -> BlockInMode CardanoMode -- ^ The block.
                    -> ChainTip                -- ^ The chain tip.
                    -> IO ()                   -- ^ Action to process transactions.
processTransactions blockHandler inHandler outHandler (BlockInMode (Block header txs) MaryEraInCardanoMode) tip =
  do
    blockHandler header tip
    sequence_
      [
        do
          sequence_
            [
              outHandler
                header
                (fromShelleyTxIn <$> toList txins)
                (TxIn (getTxId body) (TxIx ix))
                $ TxOut
                  (fromShelleyAddr address)
                  (TxOutValue supportedMultiAsset (fromMaryValue value))
            |
              (ix, txout) <- zip [0..] $ toList txouts
            , let ShelleySpec.TxOut address value = txout
            ]
          sequence_
            [
              inHandler header $ fromShelleyTxIn txin
            |
              txin <- toList txins
            ]
      |
        tx <- txs
      , let body = getTxBody tx
            ShelleyTxBody ShelleyBasedEraMary (LedgerMA.TxBody txins txouts _ _ _ _ _ _ _) _ = body
      ]
processTransactions _ _ _ _ _ = return ()
