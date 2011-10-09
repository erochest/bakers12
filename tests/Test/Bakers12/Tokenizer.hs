
module Test.Bakers12.Tokenizer (tokenizerTests) where


import qualified Data.Char as C
import qualified Data.List as L
import           Test.HUnit (Assertion, assertBool)
-- import           Test.QuickCheck
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Tokenizer
import           Text.Bakers12.Tokenizer.String ()
import qualified Text.Bakers12.Tokenizer.Xml as X


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

assertFullTokensEqual :: String -> [String] -> [[String]] -> Assertion
assertFullTokensEqual msg expected actual =
    assertBool msg . L.and $ L.zipWith (==) expTokens actual
    where expFull   = map (fullTokenize "<assertFullTokensEqual>") expected
          expTokens = map (map tokenText) expFull

-- unit     : tokenized correctly
assertFullTokenizedCorrectly :: Assertion
assertFullTokenizedCorrectly =
    assertFullTokensEqual "assertFullTokenizedCorrectly" expected actual
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
    assertFullTokensEqual "assertFullTokenizedNumbers" expected actual
    where
        expected = [ "1 2 3 4 5 3.1415 1,200,000 -33" ]
        actual   = [ ["1", "2", "3", "4", "5", "3", "1415", "1", "200", "000", "33" ] ]

-- unit     : tokenized contractions
assertFullTokenizedContractions :: Assertion
assertFullTokenizedContractions =
    assertFullTokensEqual "assertFullTokenizedContractions" expected actual
    where
        expected = [ "They'll can't isn't won't" ]
        actual   = [ ["they'll", "can't", "isn't", "won't" ] ]

-- unit     : not tokenized leading-, trailing-apostrophes
assertFullTokenizedApos :: Assertion
assertFullTokenizedApos =
    assertFullTokensEqual "assertFullTokenizedApos" expected actual
    where
        expected = [ "'tis 'will young'uns eat'?" ]
        actual   = [ ["tis", "will", "young'uns", "eat"] ]

-- Fast Tokenizer Tests:
-- property : toLower
pFastNormalized :: String -> Bool
pFastNormalized input =
    L.and [L.all isLowerAlpha token | token <- tokens]
    where tokens = fastTokenize input
          isLowerAlpha c = C.isLower c || not (C.isLetter c)

-- unit     : tokenized correctly
assertFastTokensEqual :: String -> [String] -> [[String]] -> Assertion
assertFastTokensEqual msg expected actual =
    assertBool msg . L.and $ L.zipWith (==) expTokens actual
    where expTokens = map fastTokenize expected

assertFastTokenizedCorrectly :: Assertion
assertFastTokenizedCorrectly =
    assertFastTokensEqual "assertFastTokenizedCorrectly" expected actual
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
    assertFastTokensEqual "assertFastTokenizedNumbers" expected actual
    where
        expected = [ "1 2 3 4 5 3.1415 1,200,000 -33" ]
        actual   = [ ["1", "2", "3", "4", "5", "3", "1415", "1", "200", "000", "33" ] ]

-- unit     : tokenized contractions
assertFastTokenizedContractions :: Assertion
assertFastTokenizedContractions =
    assertFastTokensEqual "assertFastTokenizedContractions" expected actual
    where
        expected = [ "They'll can't isn't won't" ]
        actual   = [ ["they'll", "can't", "isn't", "won't" ] ]

-- unit     : not tokenized leading-, trailing-apostrophes
assertFastTokenizedApos :: Assertion
assertFastTokenizedApos =
    assertFastTokensEqual "assertFastTokenizedApos" expected actual
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

-- XML:
assertXmlTokenizerCorrect :: Assertion
assertXmlTokenizerCorrect = do
    tokens <- X.fullTokenize "input" "id" input
    let actual = map makeTokenPair tokens
    assertBool "XML Tokenizer" . L.and $ L.zipWith (==) expected actual
    where
        expected = [ ("input", "this")
                   , ("input", "is")
                   , ("input#a", "the")
                   , ("input#a", "first")
                   , ("input", "day")
                   , ("input", "of")
                   , ("input#b", "the")
                   , ("input#b", "rest")
                   , ("input#c", "of")
                   , ("input#c", "my")
                   , ("input", "life")
                   ]

        input =  "<doc><p>This is <span id='a'>the first</span> "
              ++ "day of <span id='b'>the rest <span id='c'>of my</span> "
              ++ "</span> life.</p></doc>"

        makeTokenPair :: Token String -> (String, String)
        makeTokenPair tkn = (tokenSource tkn, tokenText tkn)


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
    , testGroup "XML Tokenizer" [ testCase "tokenized-xml" assertXmlTokenizerCorrect
                                ]
    , testGroup "bothTokenizers" [ testProperty "tokenizers-match" pMatchTokenizers
                                 ]
    ]



