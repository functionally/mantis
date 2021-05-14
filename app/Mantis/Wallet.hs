
{-# LANGUAGE OverloadedStrings #-}


module Mantis.Wallet (
  readAddress
, readVerificationKey
, makeVerificationKeyHash
, readSigningKey
) where


import Cardano.Api (AddressAny, AsType(..), Hash, PaymentExtendedKey, PaymentKey, SigningKey, castVerificationKey, deserialiseAddress, verificationKeyHash)
import Control.Monad.IO.Class (MonadIO)
import Mantis.Types (MantisM, foistMantisEitherIO, foistMantisExceptIO, foistMantisMaybe, throwMantis)

import qualified Cardano.CLI.Shelley.Key         as CLI (VerificationKeyTextOrFile(..), readKeyFileTextEnvelope)
import qualified Cardano.CLI.Shelley.Run.Address as CLI (SomeAddressVerificationKey(..), readAddressVerificationKeyTextOrFile)
import qualified Cardano.CLI.Types               as CLI (VerificationKeyFile(..))
import qualified Data.Text                       as T   (pack)


readAddress :: Monad m
            => String
            -> MantisM m AddressAny
readAddress =
  foistMantisMaybe "Could not deserialize address."
    . deserialiseAddress AsAddressAny
    . T.pack


readVerificationKey :: MonadIO m
                    => FilePath
                    -> MantisM m CLI.SomeAddressVerificationKey
readVerificationKey =
  foistMantisExceptIO
    . CLI.readAddressVerificationKeyTextOrFile
    . CLI.VktofVerificationKeyFile
    . CLI.VerificationKeyFile


makeVerificationKeyHash :: Monad m
                        => CLI.SomeAddressVerificationKey
                        -> MantisM m (Hash PaymentKey)
makeVerificationKeyHash (CLI.APaymentExtendedVerificationKey key) =
  return
    . verificationKeyHash
    $ castVerificationKey key
makeVerificationKeyHash _ =
  throwMantis "Unsupported address type."


readSigningKey :: MonadIO m
               => FilePath
               -> MantisM m (SigningKey PaymentExtendedKey)
readSigningKey =
  foistMantisEitherIO
    . CLI.readKeyFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
