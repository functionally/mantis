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
-- | Minting tokens.
--
-----------------------------------------------------------------------------


module Mantis.Script (
-- * Minting
  mintingScript
) where


import Cardano.Api (Hash, MaryEra, PaymentKey, SimpleScript(..), Script(..), ScriptHash, ScriptInEra(..), ScriptLanguageInEra(..), SimpleScript(..), SimpleScriptVersion(..), SlotNo, TimeLocksSupported(..), hashScript)


-- | Create a minting script.
mintingScript :: Hash PaymentKey                   -- ^ The hash of the payment key.
              -> Maybe SlotNo                      -- ^ The last slot on which minting can occur, if any.
              -> (ScriptInEra MaryEra, ScriptHash) -- ^ The script and its hash.
mintingScript hash Nothing =
  let
    script = SimpleScript SimpleScriptV2
      $ RequireSignature hash
  in
    (
      ScriptInEra SimpleScriptV2InMary script
    , hashScript script
    )
mintingScript hash (Just slot) =
  let
    script = SimpleScript SimpleScriptV2
      $ RequireAllOf
      [
        RequireSignature hash
      , RequireTimeBefore TimeLocksInSimpleScriptV2 slot
      ]
  in
    (
      ScriptInEra SimpleScriptV2InMary script
    , hashScript script
    )
