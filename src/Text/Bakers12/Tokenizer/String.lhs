\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

Text.Bakers12.Tokenizer.String

This defines an interface for Strings for the tokenizer.

\begin{code}

module Text.Bakers12.Tokenizer.String
    ( Token(..)
    , Tokenizable(..)
    , fullTokenizeFile
    , fastTokenizeFile
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import           System.IO
import           Text.Bakers12.Tokenizer

instance (Tokenizable String) where

    null = L.null

    uncons s = case s of
                    (h:t) -> Just (h, t)
                    ""    -> Nothing

    fromString = id

    toString = id

    length = L.length

    span = L.span

    dropWhile = L.dropWhile

    toLower = map C.toLower

\end{code}

These tokenize a single file. They read the content strictly, because
otherwise, attempting to tokenize a large list of files will cause an error
because too many files will be open.

\begin{code}
fullTokenizeFile :: FilePath -> IO [Token String]
fullTokenizeFile filename =
    withFile filename ReadMode $ \h -> do
        text <- hGetContents h
        L.length text `seq` return (fullTokenize filename text)

fastTokenizeFile :: FilePath -> IO [String]
fastTokenizeFile filename =
    withFile filename ReadMode $ \h -> do
        text <- hGetContents h
        L.length text `seq` return (fastTokenize text)
\end{code}

