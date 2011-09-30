
module Main (main) where

import Bakers12.Cli
import Bakers12.Snap
import Bakers12.Tokenizer
import Bakers12.Frequencies

import System.Bakers12.Utils (normalizeFilePaths)

main :: IO ()
main = do
    mode <- cmdArgs bakers12Modes
    case mode of
        Tokenize inputs idattr -> normalizeFilePaths inputs >>= tokenize idattr
        Freq inputs            -> normalizeFilePaths inputs >>= frequencies
        Serve httpPort         -> serveSnap httpPort

