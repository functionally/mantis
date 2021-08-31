
module Main (
  main
) where


import Cardano.Api  (AsType(..), ShelleyBasedEra(..))
import Paths_mantra (version)

import qualified Mantra.Command as Mantra


main :: IO ()
main =
  Mantra.main
    version
    AsMaryEra
    ShelleyBasedEraMary
