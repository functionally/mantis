{-# LANGUAGE GADTs #-}


module Mantis.Chain (
  Processor
, Reverter
, IdleNotifier
, walkBlocks
, ScriptHandler
, extractScripts
, TxInHandler
, TxOutHandler
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


type Processor =  BlockInMode CardanoMode
               -> ChainTip
               -> IO ()


type Reverter =  ChainPoint
              -> ChainTip
              -> IO ()


type IdleNotifier = IO Bool


walkBlocks :: MonadIO m
           => FilePath
           -> ConsensusModeParams CardanoMode
           -> NetworkId
           -> IdleNotifier
           -> Maybe Reverter
           -> Processor
           -> MantisM m ()
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


client :: IdleNotifier
       -> Maybe Reverter
       -> Processor
       -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
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


type ScriptHandler =  BlockHeader
                   -> Tx MaryEra
                   -> ScriptHash
                   -> Script SimpleScriptV2
                   -> IO ()


extractScripts :: MonadIO m
               => FilePath
               -> ConsensusModeParams CardanoMode
               -> NetworkId
               -> IdleNotifier
               -> (BlockHeader -> Tx MaryEra -> ScriptHash -> Script SimpleScriptV2 -> IO ())
               -> MantisM m ()
extractScripts socketPath mode network notifyIdle handler =
  walkBlocks socketPath mode network notifyIdle Nothing
    $ processScripts handler


processScripts :: (BlockHeader -> Tx MaryEra -> ScriptHash -> Script SimpleScriptV2 -> IO ())
               -> BlockInMode CardanoMode
               -> ChainTip
               -> IO ()
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


type TxInHandler =  BlockHeader
                 -> TxIn
                 -> IO ()


type TxOutHandler =  BlockHeader
                  -> [TxIn]
                  -> TxIn
                  -> TxOut MaryEra
                  -> IO ()


watchTransactions :: MonadIO m
                  => FilePath
                  -> ConsensusModeParams CardanoMode
                  -> NetworkId
                  -> Maybe Reverter
                  -> IdleNotifier
                  -> TxInHandler
                  -> TxOutHandler
                  -> MantisM m ()
watchTransactions socketPath mode network revertPoint notifyIdle inHandler outHandler =
  walkBlocks socketPath mode network notifyIdle revertPoint
    $ processTransactions inHandler outHandler


processTransactions :: TxInHandler
                    -> TxOutHandler
                    -> BlockInMode CardanoMode
                    -> ChainTip
                    -> IO ()
processTransactions inHandler outHandler (BlockInMode (Block header txs) MaryEraInCardanoMode) _tip =
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
processTransactions _ _ _ _ = return ()
