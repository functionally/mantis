
module Mantis.Command.Types (
  Mantis(..)
, Configuration(..)
, SlotRef(..)
) where


import Control.Arrow (first)
import Data.Word (Word32, Word64)


data SlotRef =
    AbsoluteSlot Integer
  | RelativeSlot Integer
    deriving (Eq, Ord, Show)

instance Read SlotRef where
  readsPrec p ('+' : remainder) = first RelativeSlot <$> readsPrec p remainder
  readsPrec p        remainder  = first AbsoluteSlot <$> readsPrec p remainder


data Configuration =
  Configuration
  {
    magic               :: Maybe Word32
  , epochSlots          :: Word64
  , addressString       :: String
  , verificationKeyFile :: FilePath
  , signingKeyFile      :: FilePath
  }
    deriving (Read, Show)


data Mantis =
    Transact
    {
      configFile   :: FilePath
    , tokenName    :: Maybe String
    , tokenCount   :: Maybe Integer
    , tokenSlot    :: Maybe SlotRef
    , metadataFile :: Maybe FilePath
    }
  | Script
    {
      configFile :: FilePath
    , scriptFile :: Maybe FilePath
    , tokenSlot  :: Maybe SlotRef
    }
  | Fingerprint
    {
      policyId  :: String
    , assetName :: String
    }
