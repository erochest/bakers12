
module Test.Bakers12.Tokenizer (tokenizerTests) where


import qualified Data.Char as C
import qualified Data.List as L
-- import qualified Data.Text as T
-- import           Test.HUnit hiding (Test)
-- import           Test.QuickCheck
import           Test.Framework (Test, testGroup)
-- import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Tokenizer
import           Text.Bakers12.Tokenizer.String ()


-- Full Tokenizer Tests:
-- property : normalized (toLower)
pFullNormalized :: String -> Bool
pFullNormalized input =
    L.and [(tokenText token) == (toLowerStr $ tokenRaw token) | token <- tokens]
    where tokens = fullTokenize "<pFullNormalized>" input
          toLowerStr = map C.toLower

-- property : length raw == tokenLength
pRawLength :: String -> Bool
pRawLength input =
    L.and [(tokenLength token) == (L.length $ tokenRaw token) | token <- tokens]
    where tokens = fullTokenize "<pRawLength>" input

-- property : tokenOffset is monotonically increasing
pOffsetIncreasing :: String -> Bool
pOffsetIncreasing input =
    let (_, ok) = L.foldl' increasing (-1, True) offsets
    in  ok
    where tokens  = fullTokenize "<pOffsetIncreasing>" input
          offsets = map tokenOffset tokens

          increasing :: (Int, Bool) -> Int -> (Int, Bool)
          increasing (prev, ok) current =
                (current, ok && prev <= current)

assertFullTokensEqual :: String -> [[String]] -> Assertion
assertFullTokensEqual expected actual =
    assertBool . L.and . map (uncurry (==)) $ L.zip expTokens actual
    where expFull   = fullTokenize "<assertFullTokensEqual>" expected
          expTokens = map tokenText expFull

-- unit     : tokenized correctly
assertFullTokenizedCorrectly :: Assertion
assertFullTokenizedCorrectly =
    assertFullTokensEqual expected actual
    where
        expected = [ "These are the days that try men's souls."
                   , "I said, \"Hi there.\""
                   , "Oh-la-la-la."
                   ]
        actual   = [ ["these", "are", "the", "days", "that", "try", "men's", "souls"]
                   , ["i", "said", "hi", "there"]
                   , ["oh", "la", "la", "la"]
                   ]

-- unit     : tokenized numbers
assertFullTokenizedNumbers :: Assertion
assertFullTokenizedNumbers =
    assertFullTokensEqual expected actual
    where
        expected = [ "1 2 3 4 5 3.1415 1,200,000 -33" ]
        actual   = [ ["1", "2", "3", "4", "5", "3", "1415", "1", "200", "000", "33" ] ]

-- unit     : tokenized contractions
assertFullTokenizedContractions :: Assertion
assertFullTokenizedContractions =
    assertFullTokensEqual expected actual
    where
        expectd = [ "They'll can't isn't won't" ]
        actual  = [ ["they'll", "can't", "isn't", "won't" ] ]

-- unit     : not tokenized leading-, trailing-apostrophes
assertFullTokenizedApos :: Assertion
assertFullTokenizedApos =
    assertFullTokenEqual expected actual
    where
        expected = [ "'tis 'will young'uns eat'?" ]
        actual   = [ ["tis", "will", "young'uns", "eat"] ]

-- Fast Tokenizer Tests:
-- property : toLower
pFastNormalized :: String -> Bool
pFastNormalized input =
    L.and [L.all isLower token | token <- tokens]
    where tokens = fastTokenizer input

-- unit     : tokenized correctly
assertFastTokensEqual :: String -> [[String]] -> Assertion
assertFastTokensEqual expected actual =
    assertBool . L.and . map (uncurry (==)) $ L.zip expTokens actual
    where expTokens = fastTokenize expected

assertFastTokenizedCorrectly :: Assertion
assertFastTokenizedCorrectly =
    assertFastTokensEqual expected actual
    where
        expected = [ "These are the days that try men's souls."
                   , "I said, \"Hi there.\""
                   , "Oh-la-la-la."
                   ]
        actual   = [ ["these", "are", "the", "days", "that", "try", "men's", "souls"]
                   , ["i", "said", "hi", "there"]
                   , ["oh", "la", "la", "la"]
                   ]

-- unit     : tokenized numbers
assertFastTokenizedNumbers :: Assertion
assertFastTokenizedNumbers =
    assertFastTokensEqual expected actual
    where
        expected = [ "1 2 3 4 5 3.1415 1,200,000 -33" ]
        actual   = [ ["1", "2", "3", "4", "5", "3", "1415", "1", "200", "000", "33" ] ]

-- unit     : tokenized contractions
assertFastTokenizedContractions :: Assertion
assertFastTokenizedContractions =
    assertFastTokensEqual expected actual
    where
        expectd = [ "They'll can't isn't won't" ]
        actual  = [ ["they'll", "can't", "isn't", "won't" ] ]

-- unit     : not tokenized leading-, trailing-apostrophes
assertFastTokenizedApos :: Assertion
assertFastTokenizedApos =
    assertFastTokenEqual expected actual
    where
        expected = [ "'tis 'will young'uns eat'?" ]
        actual   = [ ["tis", "will", "young'uns", "eat"] ]

-- Both:
-- property : normalized output is the same for both
pMatchTokenizers :: String -> Bool
pMatchTokenizers input =
    L.and [((tokenText full) == fast) | (full, fast) <- L.zip fullTokens fastTokens]
    where fullTokens = fullTokenize "<pMatchTokenizers>" input
          fastTokens = fastTokenize input


tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "fullTokenize" [ testProperty "normalized-tokens" pFullNormalized
                               , testProperty "raw-length" pRawLength
                               , testProperty "increasing-offsets" pOffsetIncreasing
                               , testCase "tokenized-correctly" assertFullTokenizedCorrectly
                               , testCase "tokenized-numbers" assertFullTokenizedNumbers
                               , testCase "tokenized-contractions" assertFullTokenizedContractions
                               , testCase "tokenized-apostrophes" assertFullTokenizedApos
                               ]
    , testGroup "fastTokenize" [ testProperty "normalized-tokens" pFastNormalized
                               , testCase "tokenized-correctly" assertFastTokenizedCorrectly
                               , testCase "tokenized-numbers" assertFastTokenizedNumbers
                               , testCase "tokenized-contractions" assertFastTokenizedContractions
                               , testCase "tokenized-apostrophes" assertFastTokenizedApos
                               ]
    , testGroup "bothTokenizers" [ testProperty "tokenizers-match" pMatchTokenizers
                                 ]
    ]



