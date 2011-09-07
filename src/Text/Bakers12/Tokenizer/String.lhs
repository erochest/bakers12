\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

Text.Bakers12.Tokenizer.String

This defines an interface for Strings for the tokenizer.

\begin{code}

module Text.Bakers12.Tokenizer.String
    ( Token(..)
    , Tokenizable(..)
    ) where

import qualified Data.Char as C
import qualified Data.List as L
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

