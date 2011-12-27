

{-| This calculates and prints the frequencies of the tokens in a set of files.
 - This really doesn't try to conserve memory, so if there are too many types,
 - they probably will fill memory.
 -}

module Bakers12.Frequencies
    ( frequencies
    ) where

import           Control.Monad (liftM)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import           System.Bakers12.Utils (isXml)
import           Text.Bakers12.Tokenizer.Text (Token(..), fastTokenizeFile)
import qualified Text.Bakers12.Tokenizer.Xml as X
import qualified Text.Bakers12.Stats as S

{-| This tokenizes the file and writes it types and their frequencies to the
 - screen as CSV.
 -}

frequencies :: [FilePath] -> IO ()
frequencies inputs = do
    (putStrLn =<<) . liftM processFreqs . mapM tokenize $ inputs

    where
        processFreqs :: [[T.Text]] -> String
        processFreqs = L.intercalate nl
                     . map showFreqInfo
                     . M.toList
                     . S.frequencies
                     . concat
        nl :: String
        nl = "\n"

        tokenize :: FilePath -> IO [T.Text]
        tokenize path | isXml path = X.fastTokenizeFile path
                      | otherwise  = fastTokenizeFile path

{-| This takes the pairs of type -> frequency and renders it as a String.
 -}

showFreqInfo :: (T.Text, Int) -> String
showFreqInfo (text, freq) = L.intercalate "," [T.unpack text, show freq]

