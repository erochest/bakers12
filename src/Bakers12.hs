
module Main (main) where

import Bakers12.Cli
import Bakers12.Tokenizer

import qualified Data.Char as C
import qualified Data.List as L
import System.Bakers12.Utils (normalizeFilePaths)
import Text.Bakers12.Tokenizer
import Text.Bakers12.Tokenizer.String ()

main :: IO ()
main = do
    mode <- cmdArgs bakers12Modes
    case mode of
        Tokenize files -> normalizeFilePaths files >>= tokenize

