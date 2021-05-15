
{-# LANGUAGE GADTs #-}


module Mantis.Query (
  queryProtocol
, queryTip
, queryUTxO
, submitTransaction
, adjustSlot
) where


import Cardano.Api (AddressAny, CardanoMode, ChainTip(..), ConsensusModeParams, EraInMode(MaryEraInCardanoMode), LocalNodeConnectInfo(..), MaryEra, NetworkId, QueryInEra(QueryInShelleyBasedEra), QueryInMode(..), ShelleyBasedEra(ShelleyBasedEraMary), SlotNo(..), Tx, TxInMode(..), TxValidationErrorInMode, getLocalChainTip, queryNodeLocalState, submitTxToNodeLocal)
import Cardano.Api.Shelley (ProtocolParameters, QueryInShelleyBasedEra(..), UTxO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Command.Types (SlotRef(..))
import Mantis.Types (MantisM, foistMantisEither, foistMantisEitherIO)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)

import qualified Data.Set as S (singleton)


queryTip
  :: MonadFail m
  => MonadIO m
  => FilePath
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> MantisM m SlotNo
queryTip socketPath mode network =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    ChainTip slotNo _ _ <- liftIO $ getLocalChainTip localNodeConnInfo
    return slotNo


adjustSlot :: SlotRef
           -> SlotNo
           -> SlotNo
adjustSlot (AbsoluteSlot slot ) _             = SlotNo $ fromIntegral slot
adjustSlot (RelativeSlot delta) (SlotNo slot) = SlotNo $ slot + fromIntegral delta


queryProtocol :: MonadFail m
              => MonadIO m
              => FilePath
              -> ConsensusModeParams CardanoMode
              -> NetworkId
              -> MantisM m ProtocolParameters
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


queryUTxO :: MonadFail m
          => MonadIO m
          => FilePath
          -> ConsensusModeParams CardanoMode
          -> AddressAny
          -> NetworkId
          -> MantisM m (UTxO MaryEra)
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
        . Just
        $ S.singleton address
    foistMantisEither utxo


submitTransaction :: MonadIO m
                  => FilePath
                  -> ConsensusModeParams CardanoMode
                  -> NetworkId
                  -> Tx MaryEra
                  -> MantisM m (SubmitResult (TxValidationErrorInMode CardanoMode))
submitTransaction socketPath mode network tx =
  do
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network socketPath
    liftIO $ submitTxToNodeLocal localNodeConnInfo
      $ TxInMode tx MaryEraInCardanoMode
