module Mantis.Chain.Internal (
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


interpretAsScript :: API.Witness era
                  -> Maybe (API.Script API.SimpleScriptV2, API.ScriptHash)
interpretAsScript witness = 
  do
    let
      text = show witness -- FIXME: Find a less crude way to deal with the existential type `Witness`.
    guard
      ("ShelleyScriptWitness ShelleyBasedEraMary (" `isPrefixOf` text)
    toScript
      <$> toSimpleScriptV2 text


toScript :: API.SimpleScript API.SimpleScriptV2
         -> (API.Script API.SimpleScriptV2, API.ScriptHash)
toScript script = 
  let
    script' = API.SimpleScript API.SimpleScriptV2 script
  in
    (
      script'
    , API.hashScript script'
    )


toSimpleScriptV2 :: String
                 -> Maybe (API.SimpleScript API.SimpleScriptV2)
toSimpleScriptV2 =
  rewriteScript
    . read
    . init
    . replace "ShelleyScriptWitness ShelleyBasedEraMary (" ""
    . replace "TimelockConstr " ""
    . replace "fromList " ""


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
