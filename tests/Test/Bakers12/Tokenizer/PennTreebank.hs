
module Test.Bakers12.Tokenizer.PennTreebank
    ( pennTreebankTests
    ) where

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           Text.Bakers12.Tokenizer


assertDoubleQuotes :: Assertion
assertDoubleQuotes = assertBool "assertDoubleQuotes" False

assertSplitFromWords :: Assertion
assertSplitFromWords = assertBool "assertSplitFromWords" False

assertEllipses :: Assertion
assertEllipses = assertBool "assertEllipses" False

assertDashes :: Assertion
assertDashes = assertBool "assertDashes" False

assertParentheses :: Assertion
assertParentheses = assertBool "assertParentheses" False

assertSquareBrackets :: Assertion
assertSquareBrackets = assertBool "assertSquareBrackets" False

assertCurlyBraces :: Assertion
assertCurlyBraces = assertBool "assertCurlyBraces" False

assertPossessives :: Assertion
assertPossessives = assertBool "assertPossessives" False

assertContractions :: Assertion
assertContractions = assertBool "assertContractions" False

assertRunOns :: Assertion
assertRunOns = assertBool "assertRunOns" False

assertRemovesWhitespace :: Assertion
assertRemovesWhitespace = assertBool "assertRemovesWhitespace" False


pennTreebankTests :: [Test]
pennTreebankTests =
    [ testGroup "punctuation"  [ testCase "double quotes" assertDoubleQuotes
                               , testCase "split from words" assertSplitFromWords
                               , testCase "ellipses" assertEllipses
                               , testCase "dashes" assertDashes
                               ]
    , testGroup "brackets"     [ testCase "parentheses" assertParentheses
                               , testCase "square brackets" assertSquareBrackets
                               , testCase "curly braces" assertCurlyBraces
                               ]
    , testGroup "contractions" [ testCase "possessives" assertPossessives
                               , testCase "contractions" assertContractions
                               , testCase "run-ons" assertRunOns
                               ]
    , testGroup "other"        [ testCase "removes whitespace" assertRemovesWhitespace
                               ]
    ]

