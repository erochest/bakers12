
-- Tests built from:
-- * http://www.cis.upenn.edu/~treebank/tokenization.html
-- * http://www.cis.upenn.edu/~treebank/tokenizer.sed

module Test.Bakers12.Tokenizer.PennTreebank
    ( pennTreebankTests
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           Text.Bakers12.Tokenizer.PennTreebank (Token(..), TokenType(..), tokenize)


tokenize' :: FilePath -> T.Text-> [Token]
tokenize' source input =
    case tokenize source input of
        Right tokens -> tokens
        Left  err    -> []

assertTokenizes :: String -> String -> [String] -> Assertion
assertTokenizes msg input expected = assertBool msg' $ expected == output
    where
        msg'   = msg ++ ": " ++ show output
        output = map (T.unpack . tokenText) . tokenize' msg $ T.pack input

assertAll :: String -> String -> (Token -> Bool) -> Assertion
assertAll msg input p = assertBool msg' all
    where
        msg'   = msg ++ ": " ++ show output
        output = tokenize' msg $ T.pack input
        all    = L.all p output


assertDoubleQuotes :: Assertion
assertDoubleQuotes = assertTokenizes "assertDoubleQuotes" input expected
    where
        input    = "\"This is quoted.\" \"More quotes.\" '\"More quotes.\"'"
        expected = [ "``", "this", "is", "quoted", ".", "''"
                   , "``", "more", "quotes", ".", "''"
                   , "'", "``", "more", "quotes", ".", "''", "'"
                   ]

assertSplitFromWords :: Assertion
assertSplitFromWords = assertTokenizes "assertSplitFromWords" input expected
    where
        input    = "some, more; other: name@other.com #tag $1.00 100% this&that"
        expected = [ "some", ",", "more", ";", "other", ":", "name", "@"
                   , "other", ".", "com", "#", "tag", "$", "1", ".", "00"
                   , "100", "%", "this", "&", "that"
                   ]

assertEllipses :: Assertion
assertEllipses = assertTokenizes "assertEllipses" input expected
    where
        input    = "Some... Others."
        expected = [ "some", "...", "others", "."
                   ]

assertDashes :: Assertion
assertDashes = assertTokenizes "assertDashes" input expected
    where
        input    = "Some--others-more"
        expected = [ "some", "--", "others", "-", "more"
                   ]

assertParentheses :: Assertion
assertParentheses = assertTokenizes "assertParentheses" input expected
    where
        input    = "(is this in parens?)"
        expected = [ "-LRB-", "is", "this", "in", "parens", "?", "-RRB-"
                   ]

assertSquareBrackets :: Assertion
assertSquareBrackets = assertTokenizes "assertSquareBrackets" input expected
    where
        input    = "[hip to be square]"
        expected = [ "-LSB-", "hip", "to", "be", "square", "-RSB-"
                   ]

assertCurlyBraces :: Assertion
assertCurlyBraces = assertTokenizes "assertCurlyBraces" input expected
    where
        input    = "{moe, larry, and}"
        expected = [ "-LCB-", "moe", ",", "larry", ",", "and", "-RCB-"
                   ]

assertPossessives :: Assertion
assertPossessives = assertTokenizes "assertPossessives" input expected
    where
        input    = "children's eric's someone else's parents'"
        expected = [ "children", "'s"
                   , "eric", "'s"
                   , "someone", "else", "'s"
                   , "parents", "'"
                   ]

assertContractions :: Assertion
assertContractions = assertTokenizes "assertContractions" input expected
    where
        input    = "won't can't musn't I'll I'm it's we'd we're we've"
        expected = [ "wo", "n't", "ca", "n't", "mus", "n't"
                   , "i", "'ll", "i", "'m"
                   , "it", "'s", "we", "'d", "we", "'re", "we", "'ve"
                   ]

assertRunOns :: Assertion
assertRunOns = assertTokenizes "assertRunOns" input expected
    where
        input    = "gonna cannot d'ye gimme 'tis wanna"
        expected = [ "gon", "na", "can", "not", "d", "ye", "gim", "me"
                   , "'t", "is", "wan", "na"
                   ]

assertRemovesWhitespace :: Assertion
assertRemovesWhitespace = assertAll "assertRemovesWhitespace" input pred
    where
        input = "some text here. i really don't care what."
        pred  = (SeparatorToken /=) . tokenType


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

