
This defines the controller and output for the tokenize mode.

\begin{code}
module Bakers12.Tokenizer
    ( tokenize
    ) where

import           Control.Monad ((=<<), liftM, mapM)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import           System.IO (readFile)
import           Text.Bakers12.Tokenizer (Token(..))
import           Text.Bakers12.Tokenizer.ByteString (fullTokenizeFile)
import           Text.Bakers12.Stats (addTypeTokenRatio)
\end{code}

This tokenzes the file and writes it to the screen as CSV.

The type transformation pipeline for this is:

        [FilePath]                          -- input
            -> IO [FilePath]                -- expand directories
            -> IO [[Token String]]          -- read and tokenize files
            -> IO [Token String]            -- concat token lists
            -> IO [(Token String, Float)]   -- type-to-token ratio decorator
            -> IO [[String]]                -- convert to row lists
            -> IO [String]                  -- convert to row strings
            -> IO String                    -- convert to output string
            -> IO ()                        -- print it out

\begin{code}
tokenize :: [FilePath] -> IO ()
tokenize inputs = do
    (putStrLn =<<) . liftM processTokens . mapM fullTokenizeFile $ inputs

    where
        processTokens :: [[Token B.ByteString]] -> String
        processTokens = L.intercalate nl
                      . map showTokenInfo
                      . addTypeTokenRatio
                      . concat
        nl :: String
        nl = "\n"
\end{code}

This takes a token and a running type-to-token ratio and turns it into a CSV
row.

\begin{code}
showTokenInfo :: (Token B.ByteString, Double) -> String
showTokenInfo (token, ttRatio) =
    L.intercalate "," [ B.unpack $ tokenText token
                      , '"' : (B.unpack $ tokenRaw token) ++ "\""
                      , tokenSource token
                      , show $ tokenOffset token
                      , show $ tokenLength token
                      , show ttRatio
                      ]
\end{code}


