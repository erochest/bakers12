
Text.Bakers12.Tokenizer.String

This defines an interface for Strings for the tokenizer.

\begin{code}

module Text.Bakers12.Tokenizer.String
    ( Token(..)
    , Tokenizable(..)
    ) where

import qualified Data.List as L
import           Text.Bakers12.Tokenizer

instance (Tokenizable String) where
    null :: String -> Bool
    null = L.null

    uncons :: String -> Maybe (Char, String)
    uncons s = case s of
                    (h:t) -> Just (h, t)
                    ""    -> Nothing

    fromString :: String -> String
    fromString = id

    length :: String -> Int
    length = L.length

    span :: (Char -> Bool) -> (String, String)
    span = L.span

    dropWhile :: (Char -> Bool) -> String -> String
    dropWhile = L.dropWhile

\end{code}

