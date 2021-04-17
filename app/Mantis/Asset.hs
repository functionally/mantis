{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Mantis.Asset (
  assetFingerprint
, assetFingerprintString
, assetFingerprintBytes
) where


import Cardano.Api.Typed (AssetId(..), AssetName(..), PolicyId(..), serialiseToRawBytes)
import Codec.Binary.Bech32 (HumanReadablePart, dataPartFromBytes, encodeLenient, humanReadablePartFromText)
import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (Blake2b_160)
import Data.ByteArray (convert)
import Data.Text (Text)
import Mantis.Types (MantisM, foistMantisEither, throwMantis)

import qualified Data.ByteString.Char8  as BS     (ByteString, pack)
import qualified Data.ByteString.Base16 as Base16 (decode)


assetPrefix :: HumanReadablePart
Right assetPrefix = humanReadablePartFromText "asset"


assetFingerprintBytes :: BS.ByteString
                      -> BS.ByteString
                      -> Text
assetFingerprintBytes policyId assetName =
  encodeLenient assetPrefix
    . dataPartFromBytes
    . convert
    . hash @_ @Blake2b_160
    $ policyId <> assetName


assetFingerprintString :: Monad m
                       => String
                       -> String
                       -> MantisM m Text
assetFingerprintString policyId assetName =
  let
    assetName' = BS.pack assetName
  in
    fmap (`assetFingerprintBytes` assetName')
     . foistMantisEither
     . Base16.decode
     $ BS.pack policyId


assetFingerprint :: Monad m
                 => AssetId
                 -> MantisM m Text
assetFingerprint (AssetId (PolicyId scriptHash) (AssetName assetName)) =
  return
    $ assetFingerprintBytes
      (serialiseToRawBytes scriptHash)
      assetName
assetFingerprint _ = throwMantis "Non-token asset."
