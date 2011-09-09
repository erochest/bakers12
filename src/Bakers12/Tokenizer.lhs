
This defines the controller and output for the tokenize mode.

\begin{code}
module Bakers12.Tokenizer
    ( modeTokenize
    , tokenize
    ) where

import qualified Data.List as L
import           System.IO (readFile)
import           Text.Bakers12.Tokenizer (Token(..), fullTokenize)
import           Text.Bakers12.Tokenizer.String ()
\end{code}

This tokenzes the file and writes it to the screen as CSV.

The type transformation pipeline for this is:

        [FilePath]                          -- input
            -> IO [FilePath]                -- expand directories
            -> IO [[Token String]]          -- read and tokenize files (may need to lift this too)
            -> IO [Token String]            -- concat token lists
            -> IO [(Token String, Float)]   -- type-to-token ratio decorator
            -> IO [[String]]                -- convert to row lists
            -> IO [String]                  -- convert to row strings
            -> IO String                    -- convert to output string
            -> IO ()                        -- print it out

TODO:

 * addTypeTokenRatio tests (Test.Bakers12.Utils)
 * addTypeTokenRatio implement (Text.Bakers12.Utils)
 * showToken
 * showTokenList

\begin{code}
modeTokenize :: [FilePath] -> IO ()
modeTokenize inputs = return ()

tokenize :: [FilePath] -> IO ()
tokenize [] = return ()
tokenize (fp:fps) = do
    text <- readFile fp
    let tokens = fullTokenize fp text :: [Token String]
    putStrLn $ showTokens tokens
    tokenize fps
\end{code}

This outputs it by writing all the parts as CSV.

\begin{code}
showTokens :: [Token String] -> String
showTokens [] = ""
showTokens (t:ts) =
    L.intercalate "," [ tokenText t
                      , '"' : (tokenRaw t) ++ "\""
                      , tokenSource t
                      , show $ tokenOffset t
                      , show $ tokenLength t
                      ] ++ ('\n' : showTokens ts)
\end{code}


