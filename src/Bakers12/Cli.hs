
{-| This defines the CLI interface for the bakers12 executable. This includes
 - command-line arguments for each mode.
 -}


module Bakers12.Cli
    ( Modes(..)
    , bakers12Modes
    , cmdArgs           -- From CmdArgs
    ) where

{-| To handle the command-line parsing, we pass everything off to cmdargs.
 -}

import System.Console.CmdArgs

{-| The main type that defines the command-line options is Modes, with one
 - constructor for each mode.
 -}

data Modes
    = Tokenize
        { files :: [FilePath]
        , idattr :: String
        }
    | Freq
        { files :: [FilePath]
        }
    | Serve
        { port :: Int
        }
    deriving (Show, Data, Typeable)

defaultPort :: Int
defaultPort = 8080

bakers12Modes :: Modes
bakers12Modes = modes
    [ Tokenize
        { files = def &= args
                      &= typ "FILES/DIRS"
        , idattr = def &= typ "ID-ATTRIBUTE"
                       &= help "The name of the attribute to look for for IDs when tokenizing XML files (default is 'id')."
                       &= name "i"
        } &= details ["This takes one or more files and tokenizes them."]
    , Freq
        { files = def &= args
                      &= typ "FILES/DIRS"
        } &= details ["This takes one or more files and calculates the frequencies for their types."]
    , Serve
        { port = defaultPort &= name "p"
        } &= details ["This starts a web server on http://localhost:8080 (or another port) for interacting with the program."]
    ] &= summary "bakers12 v0.1, (c) Eric Rochester 2011"
      &= program "bakers12"


