
-- |
-- Module      : Text.Bakers12.Tokenizer.Types
-- License     : Apache 2.0
-- Maintainer  : erochest@gmail.com
-- Portability : GHC
--
-- This defines the types for the tokenizers. Especially, this defines the
-- Token and TokenType data types.

module Text.Bakers12.Tokenizer.Types
    ( Token(..)
    , TokenType(..)
    ) where

import qualified Data.Text as T

-- * Token Type

-- | This contains data from one token instance.
data Token = Token
    { tokenText   :: T.Text         -- ^ The normalized token text.
    , tokenRaw    :: T.Text         -- ^ The raw token text.
    , tokenLength :: Int            -- ^ The length of the raw token.
    , tokenType   :: TokenType      -- ^ The type of data contained in the
                                    -- token.
    , tokenSource :: FilePath       -- ^ The source (sometimes a file) that
                                    -- the token was found in.
    , tokenOffset :: Integer        -- ^ The character offset of the start of
                                    -- the token in the source (zero-indexed).
    }
    deriving (Eq, Show)

-- | This has the types of information that a token can contain.
data TokenType =
      AlphaToken                    -- ^ Unicode alphabetic characters.
    | NumberToken                   -- ^ Unicode numeric characters.
    | SeparatorToken                -- ^ Unicode space or Unicode separator
                                    -- character.
    | PunctuationToken              -- ^ One Unicode punctuation character.
    | SymbolToken                   -- ^ One Unicode symbol character.
    | MarkToken                     -- ^ One Unicode mark character.
    | UnknownToken                  -- ^ None of the categories above.
    deriving (Eq, Show)

