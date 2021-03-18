
module Mantis.Script (
  mintingScript
) where


import Cardano.Api.Eras (MaryEra)
import Cardano.Api.Typed (Hash, PaymentKey, SimpleScript(..), Script(..), ScriptHash, ScriptInEra(..), ScriptLanguageInEra(..), SimpleScript(..), SimpleScriptVersion(..), SlotNo, TimeLocksSupported(..), hashScript)


mintingScript :: Hash PaymentKey
              -> Maybe SlotNo
              -> (ScriptInEra MaryEra, ScriptHash)
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
