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
-- | Asset utilities.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Mantra.Asset (
-- * Assets
  assetFingerprint
, assetFingerprintString
, assetFingerprintBytes
) where


import Cardano.Api            (AssetId(..), AssetName(..), PolicyId(..), serialiseToRawBytes)
import Codec.Binary.Bech32    (HumanReadablePart, dataPartFromBytes, encodeLenient, humanReadablePartFromText)
import Crypto.Hash            (hash)
import Crypto.Hash.Algorithms (Blake2b_160)
import Data.ByteArray         (convert)
import Data.Text              (Text)
import Mantra.Types           (MantraM, foistMantraEither, throwMantra)

import qualified Data.ByteString.Char8  as BS     (ByteString, pack)
import qualified Data.ByteString.Base16 as Base16 (decode)


-- | The human-readable prefix for an asset.
assetPrefix :: HumanReadablePart
Right assetPrefix = humanReadablePartFromText "asset"


-- | Compute an asset fingerprint.
assetFingerprintBytes :: BS.ByteString -- ^ The bytes of the policy ID.
                      -> BS.ByteString -- ^ The bytes of the asset name.
                      -> Text
assetFingerprintBytes policyId assetName =
  encodeLenient assetPrefix
    . dataPartFromBytes
    . convert
    . hash @_ @Blake2b_160
    $ policyId <> assetName


-- | Compute an asset fingerprint.
assetFingerprintString :: Monad m
                       => String         -- ^ The policy ID string.
                       -> String         -- ^ The asset name string.
                       -> MantraM m Text -- ^ Action for computing the fingerprint.
assetFingerprintString policyId assetName =
  let
    assetName' = BS.pack assetName
  in
    fmap (`assetFingerprintBytes` assetName')
     . foistMantraEither
     . Base16.decode
     $ BS.pack policyId


-- | Compute an asset fingerprint.
assetFingerprint :: Monad m
                 => AssetId        -- ^ The asset ID.
                 -> MantraM m Text -- ^ Action for computing the fingerprint.
assetFingerprint (AssetId (PolicyId scriptHash) (AssetName assetName)) =
  return
    $ assetFingerprintBytes
      (serialiseToRawBytes scriptHash)
      assetName
assetFingerprint _ = throwMantra "Non-token asset."
