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


module Mantra.Wallet (
-- * Keys
  SomePaymentVerificationKey
, readVerificationKey
, SomePaymentSigningKey
, readSigningKey
, makeVerificationKeyHash
-- * Addresses
, readAddress
, showAddress
, showAddressInEra
-- * Stake addresses
, stakeReference
, stakeReferenceInEra
) where


import Cardano.Api (AddressAny(..), AddressInEra(..), AddressTypeInEra(ShelleyAddressInEra), AsType(..), Hash, IsCardanoEra, IsShelleyBasedEra(..), PaymentExtendedKey, PaymentExtendedKey, PaymentKey, SigningKey, StakeAddressReference(NoStakeAddress), VerificationKey, castVerificationKey, deserialiseAddress, readFileTextEnvelope, serialiseAddress, verificationKeyHash)
import Control.Monad.IO.Class (MonadIO)
import Mantra.Types (MantraM, foistMantraEitherIO, foistMantraMaybe)

import qualified Cardano.Api.Shelley  as Shelley (Address(ShelleyAddress), fromShelleyStakeReference)
import qualified Data.Text            as T (pack, unpack)


-- | Parse an address.
readAddress :: Monad m
            => String               -- ^ The string representation.
            -> MantraM m AddressAny -- ^ Action to parse the address.
readAddress =
  foistMantraMaybe "Could not deserialize address."
    . deserialiseAddress AsAddressAny
    . T.pack


-- | Show an address.
showAddress :: AddressAny -- ^ The address.
            -> String     -- ^ The string representation.
showAddress = T.unpack . serialiseAddress


-- | Show a era-based address.
showAddressInEra :: IsCardanoEra era
                 => AddressInEra era -- ^ The address.
                 -> String           -- ^ The string representation.
showAddressInEra = T.unpack . serialiseAddress


-- | A payment verification key.
type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)


-- | Read a payment verification key.
readVerificationKey :: MonadIO m
                    => FilePath                             -- ^ Path to the key.
                    -> MantraM m SomePaymentVerificationKey -- ^ Action to read the key.
readVerificationKey file =
  foistMantraEitherIO
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


-- | A payment signing key.
type SomePaymentSigningKey = Either (SigningKey PaymentKey) (SigningKey PaymentExtendedKey)


-- | Read a signing key.
readSigningKey :: MonadIO m
               => FilePath                        -- ^ Path to the key.
               -> MantraM m SomePaymentSigningKey -- ^ Action to read the key.
readSigningKey file =
  foistMantraEitherIO
    $ do -- FIXME: Make this lazy, so the file is only read once.
      extendedKey <- fmap Right <$> readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey) file
      plainKey    <- fmap Left  <$> readFileTextEnvelope (AsSigningKey AsPaymentKey        ) file
      return $ extendedKey <> plainKey


-- | Extract a stake address from a payment address.
stakeReference :: AddressAny            -- ^ The payment address.
               -> StakeAddressReference -- ^ The stake address.
stakeReference (AddressShelley (Shelley.ShelleyAddress _ _ s)) = Shelley.fromShelleyStakeReference s
stakeReference _                                               = NoStakeAddress


-- | Extract a stake address from a payment address.
stakeReferenceInEra :: IsShelleyBasedEra era
                    => AddressInEra era     -- ^ The payment address.
                   -> StakeAddressReference -- ^ The stake address.
stakeReferenceInEra (AddressInEra (ShelleyAddressInEra _) (Shelley.ShelleyAddress _ _  s)) = Shelley.fromShelleyStakeReference s
stakeReferenceInEra _                                                                      = NoStakeAddress
