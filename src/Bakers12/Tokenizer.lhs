
This defines the controller and output for the tokenize mode.

\begin{code}
module Bakers12.Tokenizer
    ( tokenize
    ) where

import qualified Data.List as L
import           System.IO (readFile)
import           Text.Bakers12.Tokenizer (Token(..), fullTokenize)
import           Text.Bakers12.Tokenizer.String ()
\end{code}

This tokenzes the file and writes it to the screen as CSV.

\begin{code}
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


