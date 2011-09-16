
module Main where

import Test.Bakers12.Tokenizer
import Test.Bakers12.Stats

import Test.Framework (Test, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "tokenizer" tokenizerTests
    , testGroup "statistics" statsTests
    ]

