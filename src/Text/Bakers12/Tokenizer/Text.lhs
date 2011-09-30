\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

Text.Bakers12.Tokenizer.Text

This defines an interface for Data.Text for the tokenizer.

\begin{code}

module Text.Bakers12.Tokenizer.Text
    ( Token(..)
    , Tokenizable(..)
    , fullTokenizeFile
    , fastTokenizeFile
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO
import           Text.Bakers12.Tokenizer

instance (Tokenizable T.Text) where

    null = T.null

    uncons = T.uncons

    fromString = T.pack

    toString = T.unpack

    length = T.length

    span = T.span

    dropWhile = T.dropWhile

    toLower = T.toLower

\end{code}

These tokenize a single file. They read the content strictly, because
otherwise, attempting to tokenize a large list of files will cause an error
because too many files will be open.

\begin{code}
fullTokenizeFile :: String -> FilePath -> IO [Token T.Text]
fullTokenizeFile source filename =
    withFile filename ReadMode $ \h -> do
        text <- TIO.hGetContents h
        T.length text `seq` return (fullTokenize source text)

fastTokenizeFile :: FilePath -> IO [T.Text]
fastTokenizeFile filename =
    withFile filename ReadMode $ \h -> do
        text <- TIO.hGetContents h
        T.length text `seq` return (fastTokenize text)
\end{code}

