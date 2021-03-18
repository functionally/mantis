
{-# LANGUAGE OverloadedStrings #-}


module Mantis.Wallet (
  readAddress
, readVerificationKey
, makeVerificationKeyHash
, readSigningKey
) where


import Cardano.Api.Typed (AddressAny, AsType(..), Hash, PaymentExtendedKey, PaymentKey, SigningKey, castVerificationKey, deserialiseAddress, verificationKeyHash)
import Control.Monad.Except (runExceptT)

import qualified Cardano.CLI.Shelley.Key         as CLI (VerificationKeyTextOrFile(..), readKeyFileTextEnvelope)
import qualified Cardano.CLI.Shelley.Run.Address as CLI (SomeAddressVerificationKey(..), readAddressVerificationKeyTextOrFile)
import qualified Cardano.CLI.Types               as CLI (VerificationKeyFile(..))
import qualified Data.Text                       as T   (pack)


readAddress :: String -> Maybe AddressAny
readAddress = deserialiseAddress AsAddressAny . T.pack 


readVerificationKey :: FilePath -> IO CLI.SomeAddressVerificationKey
readVerificationKey filename =
  do
    let
      file = CLI.VktofVerificationKeyFile $ CLI.VerificationKeyFile filename
    result <- runExceptT $ CLI.readAddressVerificationKeyTextOrFile file
    case result of
      Left e  -> error $ show e
      Right k -> return k


makeVerificationKeyHash :: CLI.SomeAddressVerificationKey -> Hash PaymentKey
makeVerificationKeyHash (CLI.APaymentExtendedVerificationKey key) = verificationKeyHash $ castVerificationKey key
makeVerificationKeyHash _ = undefined



readSigningKey :: FilePath -> IO (SigningKey PaymentExtendedKey)
readSigningKey filename =
  do
    Right result <- CLI.readKeyFileTextEnvelope (AsSigningKey AsPaymentExtendedKey) filename
    return result
