
module Main where

import Test.Bakers12.Tokenizer
import Test.Bakers12.System.Enumerators
import Test.Framework (Test, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "tokenizer" tokenizerTests
    , testGroup "system.enumerators" systemEnumeratorTests
    ]

