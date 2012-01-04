
-- |
-- Module: Text.Bakers12.Tokenizer.PennTreebank
-- License : Apache 2.0
-- Maintainer: erochest@gmail.com
-- Portability: GHC
--
-- This filters the tokens output by Text.Bakers12.Tokenizer to make them
-- conform to the Penn Treebank tokenizer, more or less.
--
-- /More or less/ because this doesn't attempt to handle end-of-sentence
-- punctuation correctly. The original tokenizer required that each sentence be
-- on its own line. This doesn't use that.

module Text.Bakers12.Tokenizer.PennTreebank
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , pennFilter
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans (lift)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Text.Bakers12.Tokenizer as B12
import           Text.Bakers12.Tokenizer.Types

-- | This reads text from an instance of Data.Text.Text and returns a list of
-- Token instances.
tokenize :: FilePath -> T.Text -> Either SomeException [Token]
tokenize source input =
    E.runLists [[input]] process
    where process = B12.tokenizeStream source 0 E.=$ pennFilter E.=$ EL.consume

-- | This reads the input from a file and returns a list of Token instances.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (B12.tokenizeFileStream inputFile E.$= pennFilter E.$$ EL.consume)

-- | This creates an Enumerator that reads from a file and produces Tokens.
--
-- This assumes that the files are UTF8.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile =
    B12.tokenizeFileStream inputFile E.$= pennFilter

{- -- | This does some debugging tokenization. -}
{- tokenizeD :: FilePath -> T.Text -> IO (Either SomeException ()) -}
{- tokenizeD source input = -}
    {- E.run process -}
    {- where process = E.enumList 1 [input] E.$= B12.tokenizeStream source 0 -}
                    {- E.$= pennFilterIO -}
                    {- E.$$ E.printChunks True -}
                    {- -- E.$$ EL.consume -}

{- -- | This is an enumeratee that filters a token stream created by -}
{- -- Text.Bakers12.Tokenizer and makes it conform to the Penn Treebank tokenizer, -}
{- -- more or less. -}
{- pennFilterIO :: E.Enumeratee Token Token IO b -}
{- pennFilterIO cont@(E.Continue k) = do -}
    {- toDrop <- EL.takeWhile isSeparator -}
    {- lift . putStrLn . ("DROP " ++) $ show toDrop -}
    {- chunk <- EL.takeWhile (not . isSeparator) -}
    {- lift . putStrLn . ("TAKE " ++) $ show chunk -}
    {- if null chunk -}
    {- then return cont -}
    {- else do -}
        {- next  <- lift . E.runIteratee . k . E.Chunks $ penn chunk -}
        {- pennFilterIO next -}
    {- where isSeparator = (SeparatorToken ==) . tokenType -}
{- pennFilterIO step = return step -}

-- | This is an enumeratee that filters a token stream created by
-- Text.Bakers12.Tokenizer and makes it conform to the Penn Treebank tokenizer,
-- more or less.
pennFilter :: Monad m => E.Enumeratee Token Token m b
pennFilter cont@(E.Continue k) = do
    EL.dropWhile isSeparator
    chunk <- EL.takeWhile (not . isSeparator)
    case chunk of
        [] -> return cont
        _  -> do
            next  <- lift . E.runIteratee . k . E.Chunks $ penn chunk
            pennFilter next
    where isSeparator = (SeparatorToken ==) . tokenType
pennFilter step = return step

-- | This actually handles breaking everything apart and putting it back
-- together.
penn :: [Token] -> [Token]
penn [t]    | tokenText t == T.pack "\"" = [t { tokenText = T.pack "''" }]
penn (t:ts) | tokenText t == T.pack "\"" = t { tokenText = T.pack "``" } : penn ts
penn (t:ts) = t : penn ts
penn []     = []

