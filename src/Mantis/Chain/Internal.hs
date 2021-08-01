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


module Mantis.Chain.Internal (
-- * Scripts
  interpretAsScript
, toScript
, toSimpleScriptV2
) where


import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.List.Extra (replace)
import Data.Word (Word64)

import qualified Data.ByteString.Char8 as BS  (pack)
import qualified Cardano.Api           as API


-- | Convert a witness to a script.
interpretAsScript :: API.Witness era                                       -- ^ The witness.
                  -> Maybe (API.Script API.SimpleScriptV2, API.ScriptHash) -- ^ The script and its hash, if the witness was a script.
interpretAsScript witness = 
  do
    let
      text = show witness -- FIXME: Find a less crude way to deal with the existential type `Witness`.
    guard
      ("ShelleyScriptWitness ShelleyBasedEraMary (" `isPrefixOf` text)
    toScript
      <$> toSimpleScriptV2 text


-- | Convert a simple script to a script and its hash.
toScript :: API.SimpleScript API.SimpleScriptV2             -- ^ The simple script.
         -> (API.Script API.SimpleScriptV2, API.ScriptHash) -- ^ The script and its hash.
toScript script = 
  let
    script' = API.SimpleScript API.SimpleScriptV2 script
  in
    (
      script'
    , API.hashScript script'
    )


-- | Parse a string representation of a simple script.
toSimpleScriptV2 :: String                                      -- ^ The string representation.
                 -> Maybe (API.SimpleScript API.SimpleScriptV2) -- ^ The simple script, if the parsing succeeded.
toSimpleScriptV2 =
  rewriteScript
    . read
    . init
    . replace "ShelleyScriptWitness ShelleyBasedEraMary (" ""
    . replace "TimelockConstr " ""
    . replace "fromList " ""


-- Functions for parsing scripts.


rewriteScript :: Timelock -> Maybe (API.SimpleScript API.SimpleScriptV2)
rewriteScript (Signature (KeyHash h)) = API.RequireSignature <$> readSignature h
rewriteScript (AllOf Empty          ) = pure $ API.RequireAllOf []
rewriteScript (AllOf (StrictSeq ss) ) = API.RequireAllOf <$> mapM rewriteScript ss
rewriteScript (AnyOf Empty          ) = pure $ API.RequireAnyOf []
rewriteScript (AnyOf (StrictSeq ss) ) = API.RequireAnyOf <$> mapM rewriteScript ss
rewriteScript (MOfN i Empty         ) = pure $ API.RequireMOf i []
rewriteScript (MOfN i (StrictSeq ss)) = API.RequireMOf i <$> mapM rewriteScript ss
rewriteScript (TimeStart (SlotNo t) ) = pure . API.RequireTimeAfter  API.TimeLocksInSimpleScriptV2 $ API.SlotNo t
rewriteScript (TimeExpire (SlotNo t)) = pure . API.RequireTimeBefore API.TimeLocksInSimpleScriptV2 $ API.SlotNo t


readSignature :: String -> Maybe (API.Hash API.PaymentKey)
readSignature = API.deserialiseFromRawBytesHex (API.AsHash API.AsPaymentKey) . BS.pack


data Timelock =
    Signature KeyHash
  | AllOf (StrictSeq Timelock)
  | AnyOf (StrictSeq Timelock)
  | MOfN Int (StrictSeq Timelock)
  | TimeStart SlotNo
  | TimeExpire SlotNo
    deriving (Read, Show)


newtype KeyHash = KeyHash String
  deriving (Read, Show)


newtype SlotNo = SlotNo Word64
  deriving (Read, Show)


data StrictSeq a =
    Empty
  | StrictSeq {fromStrict :: [a]}
    deriving (Read, Show)
