
module Mantis.Command.Fingerprint (
  command
, main
) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Mantis.Asset (assetFingerprintString)
import Mantis.Command.Types (Mantis(Fingerprint))
import Mantis.Types (MantisM)

import qualified Options.Applicative as O
import qualified Data.Text.IO as T (putStrLn)


command :: O.Mod O.CommandFields Mantis
command =
  O.command "fingerprint"
    $ O.info options (O.progDesc "Compute the Bech32 fingerprint of a token.")


options :: O.Parser Mantis
options =
  Fingerprint
    <$> O.strArgument (O.metavar "POLICY_ID"  <> O.help "Policy ID for the token." )
    <*> O.strArgument (O.metavar "ASSET_NAME" <> O.help "Asset name for the token.")


main :: MonadIO m
     => String
     -> String
     -> MantisM m ()
main policyId assetName =
  do
    fingerprint <- assetFingerprintString policyId assetName
    liftIO $ T.putStrLn fingerprint
