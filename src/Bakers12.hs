
module Main (main) where

import Bakers12.Cli
import Bakers12.Modes


main :: IO ()
main = cmdArgs bakers12Modes >>= execBakers12

