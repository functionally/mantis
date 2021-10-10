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
-- | Extracting scripts from witnesses.
--
-----------------------------------------------------------------------------


module Mantra.Chain.Internal (
-- * Scripts
  interpretAsScript
, toScript
) where


import qualified Cardano.Api                        as API
import qualified Cardano.Api.Shelley                as Shelley
import qualified Cardano.Ledger.Crypto              as Ledger    (StandardCrypto)
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA (Timelock(..))


-- | Convert a witness to a script.
interpretAsScript :: ShelleyMA.Timelock Ledger.StandardCrypto              -- ^ The witness.
                  -> (API.SimpleScript API.SimpleScriptV2, API.ScriptHash) -- ^ The script and its hash.
interpretAsScript witness =
  let
    script = Shelley.fromAllegraTimelock API.TimeLocksInSimpleScriptV2 witness
  in
    (
      script
    , API.hashScript $ API.SimpleScript API.SimpleScriptV2 script
    )


-- Convert a timelock to a simple script.
toScript :: ShelleyMA.Timelock Ledger.StandardCrypto -- ^ The timelock.
         -> API.SimpleScript API.SimpleScriptV2      -- ^ The simple script.
toScript = Shelley.fromAllegraTimelock API.TimeLocksInSimpleScriptV2
