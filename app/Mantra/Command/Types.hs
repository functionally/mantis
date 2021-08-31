
module Mantra.Command.Types (
  Mantra(..)
, Configuration(..)
) where


import Data.Word (Word32, Word64)
import Mantra.Types (SlotRef(..))


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


data Mantra =
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
    , continue        :: Bool
    }
  | WatchAddress
    {
      configFile  :: FilePath
    , addresses   :: [String]
    , continue    :: Bool
    }
  | WatchCoin
    {
      configFile :: FilePath
    , policyId   :: String
    , assetName' :: Maybe String
    , continue   :: Bool
    }
    deriving (Eq, Ord, Read, Show)
