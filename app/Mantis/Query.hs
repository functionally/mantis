
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Mantis.Command.Types (SlotRef(..))

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
  :: Protocol
  -> NetworkId
  -> ExceptT String IO SlotNo
queryTip protocol network =
  do
    CLI.SocketPath sockPath <- firstExceptT show CLI.readEnvSocketPath
    liftIO $ withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo ->
        do
          Ouroboros.At slotNo <- Ouroboros.getTipSlotNo <$> getLocalTip connectInfo
          return slotNo


adjustSlot :: SlotRef
           -> SlotNo
           -> SlotNo
adjustSlot (AbsoluteSlot slot ) _             = SlotNo $ fromIntegral slot
adjustSlot (RelativeSlot delta) (SlotNo slot) = SlotNo $ slot + fromIntegral delta


queryProtocol
  :: Protocol
  -> NetworkId
  -> ExceptT String IO (Shelley.PParams (ShelleyLedgerEra MaryEra))
queryProtocol protocol network =
  do
    CLI.SocketPath sockPath <- firstExceptT show CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        do
          tip <- liftIO $ getLocalTip connectInfo
          Ouroboros.QueryResultSuccess pparams <- firstExceptT show . newExceptT
            $ queryNodeLocalState connectInfo
              (
                Ouroboros.getTipPoint tip
              , Ouroboros.QueryIfCurrentMary Ouroboros.GetCurrentPParams
              )
          return pparams


queryUTxO
  :: Protocol
  -> AddressAny
  -> NetworkId
  -> ExceptT String IO (Shelley.UTxO (ShelleyLedgerEra MaryEra))
queryUTxO protocol address network =
  do
    let
      address' = toShelleyAddr
        . (anyAddressInShelleyBasedEra :: AddressAny -> AddressInEra MaryEra)
        $ address
    CLI.SocketPath sockPath <- firstExceptT show CLI.readEnvSocketPath
    withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        do
          tip <- liftIO $ getLocalTip connectInfo
          Ouroboros.QueryResultSuccess utxo <- firstExceptT show . newExceptT
            $ queryNodeLocalState connectInfo
              (
                Ouroboros.getTipPoint tip
              , Ouroboros.QueryIfCurrentMary (Ouroboros.GetFilteredUTxO (S.singleton address'))
              )
          return utxo


submitTransaction
  :: Protocol
  -> NetworkId
  -> Tx MaryEra
  -> ExceptT String IO (TxSubmitResultForMode CardanoMode)
submitTransaction protocol network tx =
  do
    CLI.SocketPath sockPath <- firstExceptT show CLI.readEnvSocketPath
    liftIO
      $ withlocalNodeConnectInfo protocol network sockPath
      $ \connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} ->
        submitTx connectInfo
          . TxForCardanoMode 
          $ InAnyCardanoEra MaryEra tx
