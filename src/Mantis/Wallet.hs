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
-- | Key management.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}


module Mantis.Wallet (
-- * Keys
  SomePaymentVerificationKey
, readVerificationKey
, readSigningKey
, makeVerificationKeyHash
-- * Addresses
, readAddress
, showAddress
, showAddressMary
-- * Stake addresses
, stakeReference
, stakeReferenceMary
) where


import Cardano.Api (AddressAny(..), AddressInEra(..), AddressTypeInEra(ShelleyAddressInEra), AsType(..), Hash, MaryEra, PaymentExtendedKey, PaymentExtendedKey, PaymentKey, ShelleyBasedEra(ShelleyBasedEraMary), SigningKey, StakeAddressReference(NoStakeAddress), VerificationKey, castVerificationKey, deserialiseAddress, readFileTextEnvelope, serialiseAddress, verificationKeyHash)
import Control.Monad.IO.Class (MonadIO)
import Mantis.Types (MantisM, foistMantisEitherIO, foistMantisMaybe)

import qualified Cardano.Api.Shelley  as Shelley (Address(ShelleyAddress), fromShelleyStakeReference)
import qualified Data.Text            as T (pack, unpack)


-- | Parse an address.
readAddress :: Monad m
            => String               -- ^ The string representation.
            -> MantisM m AddressAny -- ^ Action to parse the address.
readAddress =
  foistMantisMaybe "Could not deserialize address."
    . deserialiseAddress AsAddressAny
    . T.pack


-- | Show an address.
showAddress :: AddressAny -- ^ The address.
            -> String     -- ^ The string representation.
showAddress = T.unpack . serialiseAddress


-- | Show a Mary address.
showAddressMary :: AddressInEra MaryEra -- ^ The address.
                -> String               -- ^ The string representation.
showAddressMary = T.unpack . serialiseAddress


-- | A payment verification key.
type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)


-- | Read a payment verification key.
readVerificationKey :: MonadIO m
                    => FilePath                             -- ^ Path to the key.
                    -> MantisM m SomePaymentVerificationKey -- ^ Action to read the key.
readVerificationKey file =
  foistMantisEitherIO
    $ do -- FIXME: Make this lazy, so the file is only read once.
      extendedKey <- fmap Right <$> readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) file
      plainKey    <- fmap Left  <$> readFileTextEnvelope (AsVerificationKey AsPaymentKey        ) file
      return $ extendedKey <> plainKey


-- | Compute a verification hash.
makeVerificationKeyHash :: SomePaymentVerificationKey -- ^ The key.
                        -> Hash PaymentKey            -- ^ The hash.
makeVerificationKeyHash =
  verificationKeyHash
    . either id castVerificationKey


-- | Read a signing key.
readSigningKey :: MonadIO m
               => FilePath                                  -- ^ Path to the key.
               -> MantisM m (SigningKey PaymentExtendedKey) -- ^ Action to read the key.
readSigningKey =
  foistMantisEitherIO
    . readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)


-- | Extract a stake address from a payment address.
stakeReference :: AddressAny            -- ^ The payment address.
               -> StakeAddressReference -- ^ The stake address.
stakeReference (AddressShelley (Shelley.ShelleyAddress _ _ s)) = Shelley.fromShelleyStakeReference s
stakeReference _                                               = NoStakeAddress


-- | Extract a stake address from a payment address.
stakeReferenceMary :: AddressInEra MaryEra  -- ^ The payment address.
                   -> StakeAddressReference -- ^ The stake address.
stakeReferenceMary (AddressInEra (ShelleyAddressInEra ShelleyBasedEraMary) (Shelley.ShelleyAddress _ _  s)) = Shelley.fromShelleyStakeReference s
stakeReferenceMary _                                                                                        = NoStakeAddress
