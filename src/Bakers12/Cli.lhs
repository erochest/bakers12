
This defines the CLI interface for the bakers12 executable. This includes
command-line arguments for each mode.

\begin{code}
module Bakers12.Cli
    ( Modes(..)
    , bakers12Modes
    , cmdArgs           -- From CmdArgs
    ) where
\end{code}

To handle the command-line parsing, we pass everything off to cmdargs.

\begin{code}
import System.Console.CmdArgs
\end{code}

The main type that defines the command-line options is Modes, with one
constructor for each mode.

\begin{code}
data Modes
    = Tokenize
        { files :: [FilePath]
        }
    | Serve
        { port :: Int
        }
    deriving (Show, Data, Typeable)

defaultPort :: Int
defaultPort = 8080

bakers12Modes = modes
    [ Tokenize
        { files = def &= args
                      &= typ "FILES/DIRS"
        } &= details ["This takes one or more files and tokenizes them."]
    , Serve
        { port = defaultPort &= name "p"
        } &= details ["This starts a web server on http://localhost:8080 (or another port) for interacting with the program."]
    ] &= summary "bakers12 v0.1, (c) Eric Rochester 2011"
      &= program "bakers12"
\end{code}



