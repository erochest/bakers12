{-# LANGUAGE DeriveDataTypeable #-}

{- | This defines the CLI for the bakers12 executable. This includes
 - command-line arguments for each execution mode.
 -}

module Bakers12.Cli
    ( Modes(..)
    , bakers12Modes
    , cmdArgs
    ) where

import           Bakers12.Modes.Tokenizer (TokenFilter(..))
import qualified Data.List as L
import           Data.Version (Version(..))
import           Paths_bakers12 (version)
import           Prelude hiding (filter)
import           System.Console.CmdArgs

-- | The main type that the defines the command-line options.
data Modes
    = Tokenize
        { filter :: Maybe TokenFilter
        , files  :: [FilePath]
        }
    deriving (Show, Data, Typeable)

-- | An instance of modes that defines the CLI.
bakers12Modes :: Modes
bakers12Modes = modes
    [ Tokenize
        { filter = def &= name "f" &= typ "TOKEN FILTER"
                 &= help "The filter to use on the output tokens. This can\
                          \ be one of 'null', 'minimal', 'penn'. The default\
                          \ is 'minimal'."
        , files = def &= args &= typ "FILES/DIRS"
        } &= details ["This takes one or files and tokenizes them."]
    ] &= summary ( "bakers12 v" ++ versionStr ++ (tagStrs $ versionTags version) ++
                   ", (c) Eric Rochester 2011, 2012" )
      &= program "bakers12"
    where
        versionStr :: String
        versionStr = L.intercalate "." . map show $ versionBranch version

        tagStrs :: [String] -> String
        tagStrs []     = []
        tagStrs [tag]  = tag
        tagStrs (x:xs) = '-' : x ++ tagStrs xs
        

