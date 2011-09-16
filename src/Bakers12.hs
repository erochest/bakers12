
module Main (main) where

import Bakers12.Cli
import Bakers12.Snap
import Bakers12.Tokenizer

import System.Bakers12.Utils (normalizeFilePaths)

main :: IO ()
main = do
    mode <- cmdArgs bakers12Modes
    case mode of
        Tokenize inputs -> normalizeFilePaths inputs >>= tokenize
        Serve httpPort  -> serveSnap httpPort

