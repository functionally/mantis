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
{-# LANGUAGE ScopedTypeVariables #-}


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


import Cardano.Api (Block(..), BlockHeader(..), BlockInMode(..), CardanoMode, ChainPoint, ChainTip, ConsensusModeParams, EraInMode(..), IsCardanoEra, LocalNodeClientProtocols(..), LocalChainSyncClient(..), LocalNodeConnectInfo(..), NetworkId, ScriptHash, ShelleyBasedEra(..), SimpleScript, SimpleScriptV2, TxBody(..), TxBodyContent(..), TxId, TxIn(..), TxIx(..), TxOut(..), connectToLocalNode, getTxBody, getTxId)
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..))
import Cardano.Api.Shelley          (TxBody(ShelleyTxBody))
import Control.Monad.Extra          (whenJust)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.Maybe                   (catMaybes)
import Mantis.Chain.Internal        (interpretAsScript)
import Mantis.Types                 (MantisM)

import qualified Cardano.Ledger.Crypto              as Ledger       (StandardCrypto)
import qualified Cardano.Ledger.Alonzo.Scripts      as LedgerAlonzo (Script(..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA    (Timelock)


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
type ScriptHandler =  BlockHeader                 -- ^ The block header.
                   -> TxId                        -- ^ The transaction identifier.
                   -> ScriptHash                  -- ^ The script's hash.
                   -> SimpleScript SimpleScriptV2 -- ^ The script.
                   -> IO ()                       -- ^ Action to process the script.


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
processScripts handler (BlockInMode (Block header txs) _) _ =
  sequence_
    [
      whenJust (interpretAsScript witness)
        $ \(script, hash) -> handler header txId hash script
    |
      tx <- txs
    , let body = getTxBody tx
    , let txId = getTxId body
    , witness <- extractTimelocks body
    ]


-- | Extract the time-lock scripts from a transaction body.
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
                     => BlockHandler            -- ^ Handle blocks.
                     -> TxInHandler             -- ^ Handle spent UTxOs.
                     -> TxOutHandler            -- ^ Handle transaction output.
                     -> Block era               -- ^ The block.
                     -> ChainTip                -- ^ The chain tip.
                     -> IO ()                   -- ^ Action to process transactions.
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
