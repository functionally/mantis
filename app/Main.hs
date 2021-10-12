
module Main (
  main
) where


import Cardano.Api  (ShelleyBasedEra(..))
import Paths_mantra (version)

import qualified Mantra.Command as Mantra


main :: IO ()
main =
  Mantra.main
    version
    ShelleyBasedEraAlonzo
