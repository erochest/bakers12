
This calculates and prints the frequencies of the tokens in a set of files.
This really doesn't try to conserve memory, so if there are too many types,
they probably will fill memory.

\begin{code}
module Bakers12.Frequencies
    ( frequencies
    ) where

import           Control.Monad (liftM)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Text.Bakers12.Tokenizer.Text (Token(..), fastTokenizeFile)
import qualified Text.Bakers12.Stats as S
\end{code}

This tokenizes the file and writes it types and their frequencies to the screen
as CSV.

\begin{code}
frequencies :: [FilePath] -> IO ()
frequencies inputs = do
    (putStrLn =<<) . liftM processFreqs . mapM fastTokenizeFile $ inputs

    where
        processFreqs :: [[T.Text]] -> String
        processFreqs = L.intercalate nl
                     . map showFreqInfo
                     . M.toList
                     . S.frequencies
                     . concat
        nl :: String
        nl = "\n"
\end{code}

This takes the pairs of type -> frequency and renders it as a String.

\begin{code}
showFreqInfo :: (T.Text, Int) -> String
showFreqInfo (text, freq) = L.intercalate "," [T.unpack text, show freq]
\end{code}

