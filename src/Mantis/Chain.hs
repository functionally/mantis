{-# LANGUAGE GADTs #-}


module Mantis.Chain (
  extractScripts
) where


import Cardano.Api (Block(..), BlockHeader(..), BlockInMode(..), CardanoMode, ChainPoint, ChainTip, ConsensusModeParams, EraInMode(MaryEraInCardanoMode), LocalNodeClientProtocols(..), LocalChainSyncClient(..), LocalNodeConnectInfo(..), MaryEra, NetworkId, Script, ScriptHash, SimpleScriptV2, Tx, connectToLocalNode, getTxWitnesses)
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..))
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Chain.Internal (interpretAsScript)
import Mantis.Types (MantisM)


extractScripts :: MonadIO m
               => FilePath
               -> ConsensusModeParams CardanoMode
               -> NetworkId
               -> (BlockHeader -> Tx MaryEra -> ScriptHash -> Script SimpleScriptV2 -> IO ())
               -> MantisM m ()
extractScripts socketPath mode network handler =
  walkBlocks socketPath mode network
    $ processScripts handler


type Processor =  BlockInMode CardanoMode
               -> ChainTip
               -> IO ()


walkBlocks :: MonadIO m
           => FilePath
           -> ConsensusModeParams CardanoMode
           -> NetworkId
           -> Processor
           -> MantisM m ()
walkBlocks socketPath mode network processBlock =
  let
    localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    protocols =
      LocalNodeClientProtocols
      {
        localChainSyncClient    = LocalChainSyncClient $ client processBlock
      , localTxSubmissionClient = Nothing
      , localStateQueryClient   = Nothing
      }
  in
    liftIO
      $ connectToLocalNode localNodeConnInfo protocols


client :: Processor
       -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
client processBlock =
  ChainSyncClient
    $ let
        clientStIdle =
          return
            $ SendMsgRequestNext clientStNext clientDone
        clientStNext =
          ClientStNext
          {
            recvMsgRollForward  = \block tip -> ChainSyncClient $ processBlock block tip >> clientStIdle
          , recvMsgRollBackward = \_     _   -> ChainSyncClient clientStIdle
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
