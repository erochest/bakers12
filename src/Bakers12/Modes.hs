
-- | This dispatches from the command-line Modes to the mode tasks defined as
-- submodules of this module.

module Bakers12.Modes
    ( execBakers12
    ) where

import Bakers12.Cli hiding (files)
import Bakers12.Modes.Tokenizer (TokenFilter(..), tokenize)
import Data.Maybe (fromMaybe)

-- | This dispatching function.
execBakers12 :: Modes -> IO ()
execBakers12 (Tokenize filter files) = tokenize filter' files
    where filter' = fromMaybe Minimal filter

