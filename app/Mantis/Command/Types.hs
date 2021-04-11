
module Mantis.Command.Types (
  Mantis(..)
, Configuration(..)
) where


import Data.Word (Word32, Word64)


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
    Mantis
    {
      configFile   :: FilePath
    , tokenName    :: Maybe String
    , tokenCount   :: Maybe Integer
    , tokenSlot    :: Maybe Int
    , metadataFile :: Maybe FilePath
    }
  | Fingerprint
    {
      policyId  :: String
    , assetName :: String
    }


