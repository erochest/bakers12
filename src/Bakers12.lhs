
\begin{code}
module Main (main) where
\end{code}

\begin{code}
import Bakers12.Cli

import qualified Data.Char as C
import qualified Data.List as L
import Text.Bakers12.Tokenizer
import Text.Bakers12.Tokenizer.String ()
\end{code}

\begin{code}
main :: IO ()
main = do
    let tokens = fastTokenize "1 2 3 4 5 3.1415 1,200,000 -33"
    putStrLn . ("tokens = " ++) $ show tokens
    -- putStrLn . show $ [((tokenText token), (map C.toLower $ tokenRaw token)) | token <- tokens]
    args <- cmdArgs bakers12Modes
    putStrLn $ show args
\end{code}

