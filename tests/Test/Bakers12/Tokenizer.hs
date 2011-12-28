
module Test.Bakers12.Tokenizer
    ( tokenizerTests
    ) where

import           Control.Exception (throw)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit (Assertion, assertBool)
import           Test.QuickCheck
import           Text.Bakers12.Tokenizer

instance Arbitrary T.Text where
    arbitrary =
        fmap T.pack . listOf1 $ suchThat (choose chrRange) C.isPrint
        where chrRange = (C.chr 0, C.chr maxChr)

              maxChr   :: Int
              maxChr   = 2^16

    shrink = shrinkNothing

-- This tokenizes some text and throws errors.
tokenize' :: T.Text -> [Token]
tokenize' input =
    case tokenize input of
        Right tokens -> tokens
        Left  err    -> throw err

-- tokenText must be normalized with no upper-case characters.
prop_normalized :: T.Text -> Bool
prop_normalized = L.all textIsLower . tokenize'
    where charIsLower c = (not $ C.isAlpha c) || (C.isLower c) || (C.toUpper c == c)
          textIsLower = T.all charIsLower . tokenText

-- The length of the tokens must equal the length of the raw text.
prop_tokenLength :: T.Text -> Bool
prop_tokenLength = L.all lengthIsEqualRawLength . tokenize'
    where lengthIsEqualRawLength t = tokenLength t == T.length (tokenRaw t)

-- The total length of the tokens equals the length of the raw input.
prop_totalLength :: T.Text -> Bool
prop_totalLength input = T.length input == total
    where total = L.sum . map tokenLength $ tokenize' input

-- This creates a predicate for a given token type.
isType :: TokenType -> (Token -> Bool)
isType tType = (==) tType . tokenType

-- This creates a property to test tokens of a given type against a predicate.
prop_tokenTypeContent :: TokenType -> (C.Char -> Bool) -> T.Text -> Bool
prop_tokenTypeContent tType predicate input =
    L.and [ T.all predicate $ tokenText token
          | token <- tokenize' input
          , isType tType token
          ]

-- Alphabetic tokens only contain isAlpha.
prop_isAlpha :: T.Text -> Bool
prop_isAlpha = prop_tokenTypeContent AlphaToken C.isAlpha

-- Numeric tokens only contain isNumber.
prop_isNumber :: T.Text -> Bool
prop_isNumber = prop_tokenTypeContent NumberToken C.isNumber

-- Punctuation tokens only contain isPunctuation.
prop_isPunctuation :: T.Text -> Bool
prop_isPunctuation = prop_tokenTypeContent PunctuationToken C.isPunctuation

-- Symbol tokens only contain isSymbol.
prop_isSymbol :: T.Text -> Bool
prop_isSymbol = prop_tokenTypeContent SymbolToken C.isSymbol

-- Mark tokens only contain isMark.
prop_isMark :: T.Text -> Bool
prop_isMark = prop_tokenTypeContent MarkToken C.isMark

-- Punctuation, symbol, and mark tokens are only one character long.
prop_symbolLength :: T.Text -> Bool
prop_symbolLength input =
    L.and [ 1 == tokenLength token
          | token <- tokenize' input
          , tokenType token `elem` [PunctuationToken, SymbolToken, MarkToken]
          ]

-- This is a helper function to handle the boilerplate for the unit tests.
assertTokensEqual :: String -> [[String]] -> [String] -> Assertion
assertTokensEqual msg expected actual =
    assertBool msg' . L.and $ L.zipWith (==) expected' actual'
    where
        msg' = msg ++ show actual'
        expected' = map (map T.pack) expected
        actual' = map (map tokenText . getTokens . tokenize . T.pack) actual

        getTokens (Right tokens) = tokens
        getTokens (Left _)       = []

assertAlpha :: Assertion
assertAlpha =
    assertTokensEqual "assertAlpha" expected actual
    where expected = [ [ "these", " ", "are", " ", "the", " ", "days", " "
                       , "that", " ", "try" , " ", "men", "'", "s", " "
                       , "souls", "."
                       ]
                     , [ "i", " ", "said", ",", " ", "\"", "hi", " ", "there"
                       , ".", "\""
                       ]
                     , [ "oh", "-", "la", "-", "la", "-", "la", "."
                       ]
                     ]
          actual   = [ "These are the days that try men's souls."
                     , "I said, \"Hi there.\""
                     , "Oh-la-la-la."
                     ]

assertNumber :: Assertion
assertNumber =
    assertTokensEqual "assertNumber" expected actual
    where expected = [ [ "1", " ", "2", " ", "3", " ", "4", " ", "5", " "
                       , "3", ".", "1415", " ", "1", ",", "200", ",", "000"
                       , " ", "-", "33"
                       ]
                     ]
          actual   = [ "1 2 3 4 5 3.1415 1,200,000 -33"
                     ]

assertPunctuation :: Assertion
assertPunctuation =
    assertTokensEqual "assertPunctuation" expected actual
    where expected = [ [".", ",", "\"", "&", " ", " ", " ", "*", "^"
                       ]
                     ]
          actual   = [ ".,\"&   *^"
                     ]

assertSymbol :: Assertion
assertSymbol = assertBool "assertSymbol" False

assertMark :: Assertion
assertMark = assertBool "assertMark" False

assertRange :: Assertion
assertRange = assertBool "assertRange" False

-- All the active properties and tests.
tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "properties" [ testProperty "normalized" prop_normalized
                             , testProperty "tokenLength" prop_tokenLength
                             , testProperty "totalLength" prop_totalLength
                             , testProperty "isAlpha" prop_isAlpha
                             , testProperty "isNumber" prop_isNumber
                             , testProperty "isPunctuation" prop_isPunctuation
                             , testProperty "isSymbol" prop_isSymbol
                             , testProperty "isMark" prop_isMark
                             , testProperty "symbolLength" prop_symbolLength
                             ]
    , testGroup "unittests"  [ testCase "alpha" assertAlpha
                             , testCase "number" assertNumber
                             , testCase "punctuation" assertPunctuation
                             , testCase "symbol" assertSymbol
                             , testCase "mark" assertMark
                             , testCase "range" assertRange
                             ]
    ]

