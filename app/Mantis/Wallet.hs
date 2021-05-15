
{-# LANGUAGE OverloadedStrings #-}


module Mantis.Wallet (
  SomePaymentVerificationKey
, readAddress
, readVerificationKey
, makeVerificationKeyHash
, readSigningKey
) where


import Cardano.Api (AddressAny, AsType(..), Hash, PaymentExtendedKey, PaymentExtendedKey, PaymentKey, SigningKey, VerificationKey, castVerificationKey, deserialiseAddress, readFileTextEnvelope, verificationKeyHash)
import Control.Monad.IO.Class (MonadIO)
import Mantis.Types (MantisM, foistMantisEitherIO, foistMantisMaybe)

import qualified Data.Text as T (pack)


readAddress :: Monad m
            => String
            -> MantisM m AddressAny
readAddress =
  foistMantisMaybe "Could not deserialize address."
    . deserialiseAddress AsAddressAny
    . T.pack


type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)


readVerificationKey :: MonadIO m
                    => FilePath
                    -> MantisM m SomePaymentVerificationKey
readVerificationKey file =
  foistMantisEitherIO
    $ do -- FIXME: Make this lazy, so the file is only read once.
      extendedKey <- fmap Right <$> readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) file
      plainKey    <- fmap Left  <$> readFileTextEnvelope (AsVerificationKey AsPaymentKey        ) file
      return $ extendedKey <> plainKey


makeVerificationKeyHash :: SomePaymentVerificationKey
                        -> Hash PaymentKey
makeVerificationKeyHash =
  verificationKeyHash
    . either id castVerificationKey


readSigningKey :: MonadIO m
               => FilePath
               -> MantisM m (SigningKey PaymentExtendedKey)
readSigningKey =
  foistMantisEitherIO
    . readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
