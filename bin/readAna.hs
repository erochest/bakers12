#!/usr/bin/env runhaskell
{-# LANGUAGE Arrows #-}

module Main where


import           Control.Monad (forM_, mapM_)
import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe
import           Data.Tree.NTree.TypeDefs (NTree)
import           System (getArgs)
import           Text.XML.HXT.Core

split :: Eq a => a -> [a] -> [[a]]
split on []    = []
split on input = head : split on rest
    where (head, rest) = L.break (==on) . L.dropWhile (==on) $ input

-- | main!
main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \input -> runX (configSysVars [withTrace 0, withValidate no] >>>
                                 readDocument [] input >>>
                                 processAna >>>
                                 arrIO putStrLn)

-- | This processes a file of annotations.
processAna :: IOSArrow XmlTree String
processAna = deep (isElem >>> hasName "chunklist") >>>
    proc chunkList -> do
        base    <- getAttrValue "xml:base"    -< chunkList
        chunk   <- inChildTag "chunk"         -< chunkList
        idValue <- getAttrValue "xml:base"    -< chunk
        tok     <- inChildTag "tok"           -< chunk
        href    <- getAttrValue "xlink:href"  -< tok
        token   <- text <<< inChildTag "base" -< tok
        returnA -< (showToken token base idValue $ readXlink href)

readXlink :: String -> (Int, Int)
readXlink xlink = (offset, length)
    where fields = split ',' xlink
          offset = read . (!!1) $ fields
          end    = read . L.takeWhile C.isDigit . L.dropWhile C.isSpace . (!!2) $ fields
          length = end - offset

inChildTag :: String -> IOStateArrow a XmlTree XmlTree
inChildTag tagName = getChildren >>> isElem >>> hasName tagName

text :: IOSArrow XmlTree String
text = getChildren >>> getText

showToken :: String -> String -> String -> (Int, Int) -> String
showToken token base idValue (offset, length) =
    '"':token' ++ ('"': (',': base ++ idValue) ++ (',':show offset) ++ (',':show length))
    where token' = if token == ['"'] then ['"', '"'] else token

