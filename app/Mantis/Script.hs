
{-# LANGUAGE GADTs #-}


module Mantis.Script (
  exampleScript
, exampleScriptHash
) where


import Cardano.Api.Typed (SimpleScript(..), Script(..), ScriptHash, SimpleScript(..), SimpleScriptV2, SimpleScriptVersion(..), SlotNo(..), TimeLocksSupported(..), hashScript)
import GHC.Exts (IsString(..))


exampleScript :: Script SimpleScriptV2
exampleScript =
   SimpleScript SimpleScriptV2
     $ RequireAllOf
     [
       RequireSignature $ fromString "b395c1a9464d419c69d05c148a19d44130c249abfb990c6a3fcd0b07"
     , RequireAnyOf
       [
         RequireTimeBefore TimeLocksInSimpleScriptV2 $ SlotNo 23164614
       , RequireTimeAfter  TimeLocksInSimpleScriptV2 $ SlotNo 57638214
       ]
     ]


exampleScriptHash :: ScriptHash
exampleScriptHash = hashScript exampleScript
