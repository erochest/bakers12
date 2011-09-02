
\begin{code}
module Main (main) where
\end{code}

\begin{code}
import Bakers12.Cli
\end{code}

\begin{code}
main :: IO ()
main = do
    putStrLn "Hello from Bakers12."
    args <- cmdArgs bakers12Modes
    putStrLn $ show args
\end{code}

