
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
import Mantis.Types (MantisM, foistMantisExceptIO, foistMantisEither, foistMantisEitherIO)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)

import qualified Cardano.CLI.Environment as CLI (readEnvSocketPath)
import qualified Cardano.CLI.Types       as CLI (SocketPath(..))
import qualified Data.Set                as S   (singleton)


queryTip
  :: MonadFail m
  => MonadIO m
  => ConsensusModeParams CardanoMode
  -> NetworkId
  -> MantisM m SlotNo
queryTip mode network =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network sockPath
    ChainTip slotNo _ _ <- liftIO $ getLocalChainTip localNodeConnInfo
    return slotNo


adjustSlot :: SlotRef
           -> SlotNo
           -> SlotNo
adjustSlot (AbsoluteSlot slot ) _             = SlotNo $ fromIntegral slot
adjustSlot (RelativeSlot delta) (SlotNo slot) = SlotNo $ slot + fromIntegral delta


queryProtocol :: MonadFail m
              => MonadIO m
              => ConsensusModeParams CardanoMode
              -> NetworkId
              -> MantisM m ProtocolParameters
queryProtocol mode network =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network sockPath
    pparams <-
      foistMantisEitherIO
        . queryNodeLocalState localNodeConnInfo Nothing
        . QueryInEra MaryEraInCardanoMode
        $ QueryInShelleyBasedEra ShelleyBasedEraMary QueryProtocolParameters
    foistMantisEither pparams


queryUTxO :: MonadFail m
          => MonadIO m
          => ConsensusModeParams CardanoMode
          -> AddressAny
          -> NetworkId
          -> MantisM m (UTxO MaryEra)
queryUTxO mode address network =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network sockPath
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
                  => ConsensusModeParams CardanoMode
                  -> NetworkId
                  -> Tx MaryEra
                  -> MantisM m (SubmitResult (TxValidationErrorInMode CardanoMode))
submitTransaction mode network tx =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    let
      localNodeConnInfo = LocalNodeConnectInfo mode network sockPath
    liftIO $ submitTxToNodeLocal localNodeConnInfo
      $ TxInMode tx MaryEraInCardanoMode
