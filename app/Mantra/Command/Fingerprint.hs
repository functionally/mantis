
module Mantra.Command.Fingerprint (
  command
, main
) where


import Control.Monad.IO.Class (MonadIO)
import Mantra.Asset (assetFingerprintString)
import Mantra.Command.Types (Mantra(Fingerprint))
import Mantra.Types (MantraM, printMantra)

import qualified Options.Applicative as O
import qualified Data.Text           as T (unpack)


command :: O.Mod O.CommandFields Mantra
command =
  O.command "fingerprint"
    $ O.info options (O.progDesc "Compute the Bech32 fingerprint of a token.")


options :: O.Parser Mantra
options =
  Fingerprint
    <$> O.strArgument (O.metavar "POLICY_ID"  <> O.help "Policy ID for the token." )
    <*> O.strArgument (O.metavar "ASSET_NAME" <> O.help "Asset name for the token.")


main :: MonadIO m
     => (String -> MantraM m ())
     -> String
     -> String
     -> MantraM m ()
main _ policyId assetName =
  do
    fingerprint <- assetFingerprintString policyId assetName
    printMantra $ T.unpack fingerprint
