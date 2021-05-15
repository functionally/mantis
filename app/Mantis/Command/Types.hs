
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
    socketPath          :: FilePath
  , magic               :: Maybe Word32
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
  | InfoUtxo
    {
      configFile :: FilePath
    , addresses  :: [String]
    }
  | InfoAddress
    {
      addresses :: [String]
    }
  | InfoTxBody
    {
      txBodyFiles :: [FilePath]
    }
  | InfoTx
    {
      txFiles :: [FilePath]
    }
  | Bech32Decode
    {
      bech32 :: String
    }
  | Bech32Encode
    {
      humanReadablePart :: String
    , dataPart          :: String
    }
  | Chain
    {
      configFile      :: FilePath
    , outputDirectory :: Maybe FilePath
    }
    deriving (Eq, Ord, Read, Show)
