
module Main where

import Test.Bakers12.ANC
import Test.Bakers12.Tokenizer
import Test.Bakers12.Stats

import Test.Framework (Test, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "tokenizer" tokenizerTests
    , testGroup "ANC-match" ancTests
    , testGroup "statistics" statsTests
    ]

