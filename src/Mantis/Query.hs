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
-- | Querying the blockchain and submitting transactions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GADTs #-}


module Mantis.Query (
-- * Querying
  queryProtocol
, queryTip
, queryUTxO
-- * Transactions
, submitTransaction
-- * Slots
, adjustSlot
) where


import Cardano.Api (AddressAny, CardanoMode, ChainTip(..), ConsensusModeParams, EraInMode(MaryEraInCardanoMode), LocalNodeConnectInfo(..), MaryEra, NetworkId, QueryInEra(QueryInShelleyBasedEra), QueryInMode(..), ShelleyBasedEra(ShelleyBasedEraMary), SlotNo(..), Tx, TxInMode(..), TxValidationErrorInMode, getLocalChainTip, queryNodeLocalState, submitTxToNodeLocal)
import Cardano.Api.Shelley (ProtocolParameters, QueryInShelleyBasedEra(..), QueryUTxOFilter(..), UTxO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Types (MantisM, SlotRef(..), foistMantisEither, foistMantisEitherIO)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)

import qualified Data.Set as S (singleton)


-- | Find the tip of the blockchain.
queryTip
  :: MonadFail m
  => MonadIO m
  => FilePath                        -- ^ Path to the node's socket.
  -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
  -> NetworkId                       -- ^ The network.
  -> MantisM m SlotNo                -- ^ Action to find the slot number.
queryTip socketPath mode network =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    ChainTip slotNo _ _ <- liftIO $ getLocalChainTip localNodeConnInfo
    return slotNo


-- | Compute a slot number.
adjustSlot :: SlotRef -- ^ The slot reference.
           -> SlotNo  -- ^ The current tip.
           -> SlotNo  -- ^ The resultant slot number.
adjustSlot (AbsoluteSlot slot ) _             = SlotNo $ fromIntegral slot
adjustSlot (RelativeSlot delta) (SlotNo slot) = SlotNo $ slot + fromIntegral delta


-- | Find the protocol parameters.
queryProtocol :: MonadFail m
              => MonadIO m
              => FilePath                        -- ^ Path to the node's socket.
              -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
              -> NetworkId                       -- ^ The network.
              -> MantisM m ProtocolParameters    -- ^ Action to find the protocol parameters.
queryProtocol socketPath mode network =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    pparams <-
      foistMantisEitherIO
        . queryNodeLocalState localNodeConnInfo Nothing
        . QueryInEra MaryEraInCardanoMode
        $ QueryInShelleyBasedEra ShelleyBasedEraMary QueryProtocolParameters
    foistMantisEither pparams


-- | Find UTxOs at an address.
queryUTxO :: MonadFail m
          => MonadIO m
          => FilePath                        -- ^ Path to the node's socket.
          -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
          -> AddressAny                      -- ^ The address.
          -> NetworkId                       -- ^ The network.
          -> MantisM m (UTxO MaryEra)        -- ^ Action to find the UTxOs.
queryUTxO socketPath mode address network =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    utxo <-
      foistMantisEitherIO
        . queryNodeLocalState localNodeConnInfo Nothing
        . QueryInEra MaryEraInCardanoMode
        . QueryInShelleyBasedEra ShelleyBasedEraMary
        . QueryUTxO
        . QueryUTxOByAddress
        $ S.singleton address
    foistMantisEither utxo


-- | Submit a transaction.
submitTransaction :: MonadIO m
                  => FilePath                                                       -- ^ Path to the node's socket.
                  -> ConsensusModeParams CardanoMode                                -- ^ The consensus mode.
                  -> NetworkId                                                      -- ^ The network.
                  -> Tx MaryEra                                                     -- ^ The transaction.
                  -> MantisM m (SubmitResult (TxValidationErrorInMode CardanoMode)) -- ^ Action to submit the transaction and return its result.
submitTransaction socketPath mode network tx =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    liftIO $ submitTxToNodeLocal localNodeConnInfo
      $ TxInMode tx MaryEraInCardanoMode
