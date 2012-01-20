{-# LANGUAGE DeriveDataTypeable #-}

-- | This is the controller for the `tokenizer` mode. It runs the tokenizer and
-- prints out the output.

module Bakers12.Modes.Tokenizer
    ( tokenize
    , TokenFilter(..)
    ) where

import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import           Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           System.Bakers12.Enumerators (removeMissingFiles, expandDirectories)
import           System.Console.CmdArgs (Data, Typeable)
import           System.IO (stdout)
import           Text.Bakers12.Csv (toCSVE)
import           Text.Bakers12.Tokenizer (Token(..), tokenizeE)
import           Text.Bakers12.Tokenizer.Csv ()
import           Text.Bakers12.Tokenizer.Minimal (minimalFilter)
import           Text.Bakers12.Tokenizer.PennTreebank (pennFilter)


-- | This is an enumeration of the types of token filters provided by other
-- modules.
data TokenFilter
    = Null
    | Minimal
    | Penn
    deriving (Data, Enum, Eq, Show, Typeable)

-- | This takes a list of possible file paths and tokenizes each one. It prints
-- the tokens out as CSV. Missing files are silently skipped and directories
-- are expanded into all the files in that directory and subdirectories. All of
-- this is handled with Enumerators, so it's memory consumption should be
-- decent.
tokenize :: TokenFilter -> [FilePath] -> IO ()
tokenize tokenFilter files =
    run_ (tokenEnum' $$ outputIter)
    where
        fileEnum   = enumLists [files] $= removeMissingFiles $= expandDirectories
        tokenEnum  = fileEnum $= tokenizeE
        tokenEnum' = case tokenFilter of
                        Null    -> tokenEnum
                        Minimal -> tokenEnum $= minimalFilter
                        Penn    -> tokenEnum $= pennFilter
        outputIter = toCSVE =$
                     ET.encode ET.utf8 =$
                     EB.iterHandle stdout

