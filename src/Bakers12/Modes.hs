
-- | This dispatches from the command-line Modes to the mode tasks defined as
-- submodules of this module.

module Bakers12.Modes
    ( execBakers12
    ) where

import Bakers12.Cli hiding (files)
import Bakers12.Modes.Tokenizer (tokenize)

-- | This dispatching function.
execBakers12 :: Modes -> IO ()
execBakers12 (Tokenize files) = tokenize files

