
\begin{code}
module Main (main) where
\end{code}

\begin{code}
import Bakers12.Cli
import Bakers12.Tokenizer

import qualified Data.Char as C
import qualified Data.List as L
import Text.Bakers12.Tokenizer
import Text.Bakers12.Tokenizer.String ()
\end{code}

\begin{code}
main :: IO ()
main = do
    mode <- cmdArgs bakers12Modes
    case mode of
        Tokenize files -> tokenize files
\end{code}

