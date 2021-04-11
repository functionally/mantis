
module Main (
  main
) where


import Paths_mantis (version)

import qualified Mantis.Command as Mantis


main :: IO ()
main = Mantis.main version
