
module Main where

import Test.Bakers12.System.Enumerators
import Test.Bakers12.Tokenizer
import Test.Bakers12.Tokenizer.PennTreebank
import Test.Framework (Test, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "tokenizer" tokenizerTests
    , testGroup "penn treebank" pennTreebankTests
    , testGroup "system.enumerators" systemEnumeratorTests
    ]

