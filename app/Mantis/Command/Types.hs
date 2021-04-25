
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
      configFile    :: FilePath
    , tokenName     :: Maybe String
    , tokenCount    :: Maybe Integer
    , tokenSlot     :: Maybe SlotRef
    , outputAddress :: Maybe String
    , scriptFile    :: Maybe FilePath
    , metadataFile  :: Maybe FilePath
    }
  | Mint
    {
      configFile    :: FilePath
    , mintingFile   :: FilePath
    , tokenSlot     :: Maybe SlotRef
    , outputAddress :: Maybe String
    , scriptFile    :: Maybe FilePath
    , metadataFile  :: Maybe FilePath
    }
  | Script
    {
      configFile :: FilePath
    , tokenSlot  :: Maybe SlotRef
    , scriptFile :: Maybe FilePath
    }
  | Fingerprint
    {
      policyId  :: String
    , assetName :: String
    }
  | Info
    {
      configFile :: FilePath
    , outputAddress :: Maybe String
    , txBodyFile    :: Maybe FilePath
    , txFile        :: Maybe FilePath
    }
    deriving (Eq, Ord, Read, Show)
