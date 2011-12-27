{-# LANGUAGE TypeSynonymInstances #-}

{-| Text.Bakers12.Tokenizer.ByteString
 - 
 - This defines an interface for lazy and strict Char8 ByteStrings for the
 - tokenizer.
 -}

module Text.Bakers12.Tokenizer.ByteString
    ( Token(..)
    , Tokenizable(..)
    , fullTokenizeFile
    , fastTokenizeFile
    , fullLazyTokenizeFile
    , fastLazyTokenizeFile
    ) where

import qualified Data.Char as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           System.IO
import           Text.Bakers12.Tokenizer

{-| First, we instantiate the strict ByteString type as Tokenizable. And define
 - functions to read tokens from files using strict ByteStrings.
 -}

instance (Tokenizable B.ByteString) where

    null = B.null

    uncons = B.uncons

    fromString = B.pack

    toString = B.unpack

    length = B.length

    span = B.span

    dropWhile = B.dropWhile

    toLower = B.map C.toLower

fullTokenizeFile :: String -> FilePath -> IO [Token B.ByteString]
fullTokenizeFile source filename =
    withFile filename ReadMode $ \h -> do
        text <- B.hGetContents h
        B.length text `seq` return (fullTokenize source text)

fastTokenizeFile :: FilePath -> IO [B.ByteString]
fastTokenizeFile filename =
    withFile filename ReadMode $ \h -> do
        text <- B.hGetContents h
        B.length text `seq` return (fastTokenize text)

{-| Now, we instantiate a lazy ByteString type as Tokenizable.
 -}

instance (Tokenizable L.ByteString) where

    null = L.null

    uncons = L.uncons

    fromString = L.pack

    toString = L.unpack

    length = fromIntegral . L.length

    span = L.span

    dropWhile = L.dropWhile

    toLower = L.map C.toLower

fullLazyTokenizeFile :: String -> FilePath -> IO [Token L.ByteString]
fullLazyTokenizeFile source filename =
    withFile filename ReadMode $ \h -> do
        text <- L.hGetContents h
        L.length text `seq` return (fullTokenize source text)

fastLazyTokenizeFile :: FilePath -> IO [L.ByteString]
fastLazyTokenizeFile filename =
    withFile filename ReadMode $ \h -> do
        text <- L.hGetContents h
        L.length text `seq` return (fastTokenize text)

