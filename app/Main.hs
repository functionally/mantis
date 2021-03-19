
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Cardano.Api (NetworkId(..), getTxId)
import Cardano.Api.Protocol (Protocol(..))
import Cardano.Api.Eras (CardanoEra(MaryEra))
import Cardano.Api.Shelley (ShelleyWitnessSigningKey(..), TxOut(..), fromMaryValue, TxOutValue(..), makeScriptWitness, makeSignedTransaction, makeShelleyKeyWitness)
import Cardano.Api.Typed (NetworkMagic(..), SlotNo(..), anyAddressInEra, makeTransactionBody)
import Control.Monad.Except (runExceptT)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Version (showVersion)
import Data.Word (Word32, Word64)
import Mantis.Query (queryProtocol, queryTip, queryUTxO, submitTransaction)
import Mantis.Transaction (fromShelleyUTxO, includeFee, makeMinting, makeTransaction, printUTxO, printValue, readMetadata, summarizeValues, supportedMultiAsset)
import Mantis.Wallet (makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import Paths_mantis (version)

import qualified Cardano.Chain.Slotting     as Chain   (EpochSlots(..))
import qualified Data.ByteString.Lazy.Char8 as LBS     (unpack)
import qualified Data.Map.Strict            as M       (keys)
import qualified Options.Applicative        as O


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


data Mantis = Mantis
  {
    configFile   :: FilePath
  , tokenName    :: Maybe String
  , tokenCount   :: Maybe Integer
  , tokenSlot    :: Maybe Int
  , metadataFile :: Maybe FilePath
  }


main :: IO ()
main =
  do
    let
      parser =
        O.info
          (O.helper <*> versionOption <*> programOptions)
          (O.fullDesc <> O.progDesc "Submit Cardano metadata or mint Cardano tokens." <> O.header "Mantis Cardano tool.")
      versionOption = O.infoOption ("Mantis " ++ showVersion version) (O.long "version" <> O.help "Show version.")
      programOptions =
        Mantis
          <$>             O.strArgument   ( O.metavar "CONFIG"    <> O.help "Path to configuration file."                                       )
          <*> O.optional (O.strArgument   $ O.metavar "TOKEN"     <> O.help "Name of token to mint or burn."                                    )
          <*> O.optional (O.option O.auto $ O.long    "count"     <> O.help "Number of tokens to mint or burn."                                 )
          <*> O.optional (O.option O.auto $ O.long    "before"    <> O.help "Number of slots into the future when tokens are mintable/burnable.")
          <*> O.optional (O.strOption     $ O.long    "metadata"  <> O.help "Path to metadata JSON file."                                       )
    Mantis{..} <- O.execParser parser

    Configuration{..} <- read <$> readFile configFile
    let
      protocol = CardanoProtocol $ Chain.EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
    putStrLn ""
    putStrLn $ "Network: " ++ show network

    Right (SlotNo tip) <- runExceptT $ queryTip protocol network
    putStrLn ""
    putStrLn $ "Tip slot: " ++ show tip

    Right pparams <- runExceptT $ queryProtocol protocol network
    putStrLn ""
    putStrLn $ "Protocol parameters: " ++ LBS.unpack (encode pparams)

    let
      Just address = readAddress addressString
    putStrLn ""
    putStrLn $ "Address: " ++ addressString

    verificationKey <- readVerificationKey verificationKeyFile
    let
      verificationKeyHash = makeVerificationKeyHash verificationKey
    signingKey <- readSigningKey signingKeyFile
    putStrLn ""
    putStrLn $ "Verification key hash: " ++ show verificationKeyHash
    putStrLn "Signing key . . . read successfuly."

    putStrLn ""
    putStrLn "Unspect UTxO:"
    Right utxo <- runExceptT $ queryUTxO protocol address network
    printUTxO "  " utxo

    let
      (nIn, value) = summarizeValues utxo
    putStrLn ""
    putStrLn "Total value:"
    printValue "  " value

    metadata <- sequence $ readMetadata <$> metadataFile
    putStrLn ""
    putStrLn "Metadata . . . read and parsed."

    let
      (script, minting, value') =
        case tokenName of
          Nothing   -> (Nothing, Nothing, fromMaryValue value)
          Just name -> let
                         (script', minting', value'') = makeMinting
                                                          value
                                                          name
                                                          (fromMaybe 1 tokenCount)
                                                          verificationKeyHash
                                                          $ SlotNo . (tip +) . toEnum <$> tokenSlot
                       in
                         (Just script', Just minting', value'')
    putStrLn ""
    putStrLn $ "Policy: " ++ show script
    putStrLn ""
    putStrLn $ "Minting: " ++ show minting

    let
      Just address' = anyAddressInEra MaryEra address
      txBody = includeFee network pparams nIn 1 1 0
        $ makeTransaction 
          (M.keys $ fromShelleyUTxO utxo)
          [TxOut address' (TxOutValue supportedMultiAsset value')]
          metadata
          Nothing
          minting
      Right txRaw = makeTransactionBody txBody
    putStrLn ""
    putStrLn $ "Transaction: " ++ show txRaw

    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signingKey
      witness' = makeScriptWitness <$> script
      txSigned = makeSignedTransaction (witness : maybeToList witness') txRaw
    print witness
    result <- runExceptT $ submitTransaction protocol network txSigned
    putStrLn ""
    putStrLn $ "Result: " ++ show result
    putStrLn $ "TxID: " ++ show (getTxId txRaw)
