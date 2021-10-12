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


{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Mantra.Query (
-- * Querying
  queryProtocol
, queryTip
, queryUTxO
-- * Transactions
, submitTransaction
-- * Slots
, adjustSlot
) where


import Cardano.Api                                       (AddressAny, CardanoMode, ChainTip(..), ConsensusMode(CardanoMode), ConsensusModeParams, IsCardanoEra(..), IsShelleyBasedEra(..), LocalNodeConnectInfo(..), NetworkId, QueryInEra(QueryInShelleyBasedEra), QueryInMode(..), ShelleyBasedEra, SlotNo(..), Tx, TxInMode(..), TxValidationErrorInMode, getLocalChainTip, queryNodeLocalState, shelleyBasedToCardanoEra, submitTxToNodeLocal, toEraInMode)
import Cardano.Api.Shelley                               (ProtocolParameters, QueryInShelleyBasedEra(..), QueryUTxOFilter(..), UTxO)
import Control.Monad.IO.Class                            (MonadIO, liftIO)
import Mantra.Types                                      (MantraM, SlotRef(..), foistMantraEither, foistMantraEitherIO)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)

import qualified Data.Set as S (singleton)


-- | Find the tip of the blockchain.
queryTip
  :: MonadFail m
  => MonadIO m
  => FilePath                        -- ^ Path to the node's socket.
  -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
  -> NetworkId                       -- ^ The network.
  -> MantraM m SlotNo                -- ^ Action to find the slot number.
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
queryProtocol :: IsShelleyBasedEra era
              => MonadFail m
              => MonadIO m
              => ShelleyBasedEra era             -- ^ The era.
              -> FilePath                        -- ^ Path to the node's socket.
              -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
              -> NetworkId                       -- ^ The network.
              -> MantraM m ProtocolParameters    -- ^ Action to find the protocol parameters.
queryProtocol sbe socketPath mode network =
  do
    let
      Just eraInMode = toEraInMode (shelleyBasedToCardanoEra sbe) CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    pparams <-
      foistMantraEitherIO
        . queryNodeLocalState localNodeConnInfo Nothing
        . QueryInEra eraInMode
        $ QueryInShelleyBasedEra shelleyBasedEra QueryProtocolParameters
    foistMantraEither pparams


-- | Find UTxOs at an address.
queryUTxO :: IsCardanoEra era
          => MonadFail m
          => MonadIO m
          => ShelleyBasedEra era             -- ^ The era.
          -> FilePath                        -- ^ Path to the node's socket.
          -> ConsensusModeParams CardanoMode -- ^ The consensus mode.
          -> AddressAny                      -- ^ The address.
          -> NetworkId                       -- ^ The network.
          -> MantraM m (UTxO era)            -- ^ Action to find the UTxOs.
queryUTxO sbe socketPath mode address network =
  do
    let
      Just eraInMode = toEraInMode (shelleyBasedToCardanoEra sbe) CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    utxo <-
      foistMantraEitherIO
        . queryNodeLocalState localNodeConnInfo Nothing
        . QueryInEra eraInMode
        . QueryInShelleyBasedEra sbe
        . QueryUTxO
        . QueryUTxOByAddress
        $ S.singleton address
    foistMantraEither utxo


-- | Submit a transaction.
submitTransaction :: IsCardanoEra era
                  => MonadIO m
                  => ShelleyBasedEra era                                            -- ^ The era.
                  -> FilePath                                                       -- ^ Path to the node's socket.
                  -> ConsensusModeParams CardanoMode                                -- ^ The consensus mode.
                  -> NetworkId                                                      -- ^ The network.
                  -> Tx era                                                         -- ^ The transaction.
                  -> MantraM m (SubmitResult (TxValidationErrorInMode CardanoMode)) -- ^ Action to submit the transaction and return its result.
submitTransaction sbe socketPath mode network tx =
  do
    let
      Just eraInMode = toEraInMode (shelleyBasedToCardanoEra sbe) CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    liftIO $ submitTxToNodeLocal localNodeConnInfo
      $ TxInMode tx eraInMode
