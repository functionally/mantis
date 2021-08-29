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


import Cardano.Api (Hash, PaymentKey, SimpleScript(..), Script(..), ScriptHash, SimpleScript(..), SimpleScriptV2, SimpleScriptVersion(..), SlotNo, TimeLocksSupported(..), hashScript)


-- | Create a minting script.
mintingScript :: Hash PaymentKey                           -- ^ The hash of the payment key.
              -> Maybe SlotNo                              -- ^ The last slot on which minting can occur, if any.
              -> (SimpleScript SimpleScriptV2, ScriptHash) -- ^ The script and its hash.
mintingScript hash Nothing =
  let
    script = RequireSignature hash
  in
    (
      script
    , hashScript $ SimpleScript SimpleScriptV2 script
    )
mintingScript hash (Just slot) =
  let
    script =
      RequireAllOf
        [
          RequireSignature hash
        , RequireTimeBefore TimeLocksInSimpleScriptV2 slot
        ]
  in
    (
      script
    , hashScript $ SimpleScript SimpleScriptV2 script
    )
