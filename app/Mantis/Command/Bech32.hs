
module Mantis.Command.Bech32 (
  command
, mainDecode
, mainEncode
) where


import Control.Monad.IO.Class (MonadIO)
import Mantis.Command.Types (Mantis(Bech32Decode, Bech32Encode))
import Mantis.Types (MantisM, foistMantisEither, foistMantisMaybe, printMantis)

import qualified Codec.Binary.Bech32    as Bech32 (dataPartFromBytes, dataPartToBytes, decodeLenient, encodeLenient, humanReadablePartFromText, humanReadablePartToText)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString.Char8  as BS (pack, unpack)
import qualified Options.Applicative    as O
import qualified Data.Text              as T (pack, unpack)


command :: O.Mod O.CommandFields Mantis
command =
  mconcat
    [
      O.command "bech32-decode" $ O.info
        (
          Bech32Decode
            <$> O.strArgument (O.metavar "BECH32" <> O.help "The Bech32 text." )
        )
        (O.progDesc "Decode a Bech32 string.")
    , O.command "bech32-encode" $ O.info
        (
          Bech32Encode
            <$> O.strArgument (O.metavar "PREFIX" <> O.help "The human-readable part." )
            <*> O.strArgument (O.metavar "DATA"   <> O.help "The data part.")
        )
        (O.progDesc "Encode a Bech32 string.")
    ]


mainDecode :: MonadIO m
           => (String -> MantisM m ())
           -> String
           -> MantisM m ()
mainDecode debugMantis text =
  do
    (humanReadablePart, dataPart) <-
      foistMantisEither
        . Bech32.decodeLenient
        $  T.pack text
    let
      humanReadablePart' =
        T.unpack
          $ Bech32.humanReadablePartToText humanReadablePart
    dataPart' <-
      foistMantisMaybe "Failed decoding data part."
        $ BS.unpack . Base16.encode
        <$> Bech32.dataPartToBytes dataPart
    debugMantis $ "Human-readable part: " ++ humanReadablePart'
    printMantis dataPart'


mainEncode :: MonadIO m
           => (String -> MantisM m ())
           -> String
           -> String
           -> MantisM m ()
mainEncode _ humanReadablePart dataPart =
  do
    humanReadablePart' <-
      foistMantisEither
        . Bech32.humanReadablePartFromText
        $ T.pack humanReadablePart
    datapart' <-
      foistMantisEither
        . fmap Bech32.dataPartFromBytes
        . Base16.decode
        $ BS.pack dataPart
    let
      encoded =
        T.unpack
          $ Bech32.encodeLenient humanReadablePart' datapart'
    printMantis encoded
