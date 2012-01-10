
-- |
-- Module: Text.Bakers12.Tokenizer.Minimal
-- License: Apache 2.0
-- Maintainer: erochest@gmail.com
-- Portability: GHC
--
-- This is a minimal tokenizer. It just removes whitespace and separators from
-- the token stream.

module Text.Bakers12.Tokenizer.Minimal
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , minimalFilter
    ) where

import           Control.Exception (SomeException)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Text.Bakers12.Tokenizer as B12
import           Text.Bakers12.Tokenizer.Types hiding (append, concat)
import qualified Text.Bakers12.Tokenizer.Types as Tkn

-- | This reads text from an instsance of Data.Text.Text and returns a list of
-- Token instances.
tokenize :: FilePath -> T.Text -> Either SomeException [Token]
tokenize source input =
    E.runLists [[input]] process
    where process = B12.tokenizeStream source 0 E.=$ minimalFilter E.=$ EL.consume

-- | This reads the input from a file and returns a list of Token instances.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (B12.tokenizeFileStream inputFile E.$= minimalFilter E.$$ EL.consume)

-- | This creates an Enumerator that reads from a file and produces Tokens.
--
-- This assumes the files are UTF8.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile =
    B12.tokenizeFileStream inputFile E.$= minimalFilter

-- | This is an enumeratee that filters a token stream created by
-- Text.Bakers12.Tokenizer and basically just removes the whitespace and
-- separator characters from it.
minimalFilter :: Monad m => E.Enumeratee Token Token m b
minimalFilter = EL.filter ((SeparatorToken /=) . tokenType)

