
{-# LANGUAGE GADTs #-}


module Mantis.Query (
  queryProtocol
, queryTip
, queryUTxO
, submitTransaction
, adjustSlot
) where


import Cardano.Api.Eras (CardanoEra(..), InAnyCardanoEra(..), MaryEra, ShelleyLedgerEra)
import Cardano.Api.LocalChainSync (getLocalTip)
import Cardano.Api.Protocol (Protocol, withlocalNodeConnectInfo)
import Cardano.Api.Shelley (anyAddressInShelleyBasedEra, toShelleyAddr)
import Cardano.Api.TxSubmit (TxForMode(..), TxSubmitResultForMode, submitTx)
import Cardano.Api.Typed (AddressAny, AddressInEra(..), CardanoMode, LocalNodeConnectInfo(..), NetworkId, NodeConsensusMode(..), SlotNo(..), Tx, queryNodeLocalState)
import Control.Monad.IO.Class (MonadIO)
import Mantis.Command.Types (SlotRef(..))
import Mantis.Types (MantisM, foistMantisIO, foistMantisExceptIO, foistMantisEitherIO)

import qualified Cardano.CLI.Environment            as CLI       (readEnvSocketPath)
import qualified Cardano.CLI.Types                  as CLI       (SocketPath(..))
import qualified Data.Set                           as S         (singleton)
import qualified Ouroboros.Consensus.Cardano.Block  as Ouroboros (Either (..), Query (..))
import qualified Ouroboros.Consensus.Shelley.Ledger as Ouroboros (Query(..))
import qualified Ouroboros.Network.Block            as Ouroboros (getTipPoint, getTipSlotNo)
import qualified Ouroboros.Network.Point            as Ouroboros (WithOrigin(At))
import qualified Shelley.Spec.Ledger.PParams        as Shelley   (PParams)
import qualified Shelley.Spec.Ledger.UTxO           as Shelley   (UTxO (..))


queryTip
  :: MonadFail m
  => MonadIO m
  => Protocol
  -> NetworkId
  -> MantisM m SlotNo
queryTip protocol network =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo ->
        do
          Ouroboros.At slotNo <-
            foistMantisIO
              $ Ouroboros.getTipSlotNo
              <$> getLocalTip connectInfo
          return slotNo


adjustSlot :: SlotRef
           -> SlotNo
           -> SlotNo
adjustSlot (AbsoluteSlot slot ) _             = SlotNo $ fromIntegral slot
adjustSlot (RelativeSlot delta) (SlotNo slot) = SlotNo $ slot + fromIntegral delta


queryProtocol :: MonadFail m
              => MonadIO m
              => Protocol
              -> NetworkId
              -> MantisM m (Shelley.PParams (ShelleyLedgerEra MaryEra))
queryProtocol protocol network =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        do
          tip <- foistMantisIO $ getLocalTip connectInfo
          Ouroboros.QueryResultSuccess pparams <- foistMantisEitherIO
            $ queryNodeLocalState connectInfo
              (
                Ouroboros.getTipPoint tip
              , Ouroboros.QueryIfCurrentMary Ouroboros.GetCurrentPParams
              )
          return pparams


queryUTxO :: MonadFail m
          => MonadIO m
          => Protocol
          -> AddressAny
          -> NetworkId
          -> MantisM m (Shelley.UTxO (ShelleyLedgerEra MaryEra))
queryUTxO protocol address network =
  do
    let
      address' = toShelleyAddr
        . (anyAddressInShelleyBasedEra :: AddressAny -> AddressInEra MaryEra)
        $ address
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        do
          tip <- foistMantisIO $  getLocalTip connectInfo
          Ouroboros.QueryResultSuccess utxo <- foistMantisEitherIO
            $ queryNodeLocalState connectInfo
              (
                Ouroboros.getTipPoint tip
              , Ouroboros.QueryIfCurrentMary (Ouroboros.GetFilteredUTxO (S.singleton address'))
              )
          return utxo


submitTransaction :: MonadIO m
                  => Protocol
                  -> NetworkId
                  -> Tx MaryEra
                  -> MantisM m (TxSubmitResultForMode CardanoMode)
submitTransaction protocol network tx =
  do
    CLI.SocketPath sockPath <- foistMantisExceptIO CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        foistMantisIO
          . submitTx connectInfo
          . TxForCardanoMode 
          $ InAnyCardanoEra MaryEra tx
