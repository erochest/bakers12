
module Test.Bakers12.Stats (statsTests) where

import qualified Data.List as L
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Tokenizer
import           Text.Bakers12.Tokenizer.String ()
import           Text.Bakers12.Stats

-- property: ratio is always <= 1
pFullRatioFractional :: String -> Bool
pFullRatioFractional input =
    L.and . map (1.0 >=) $ ratios
    where
        tokens = addTypeTokenRatio . fullTokenize "<pRatioFractional>" $ input
        ratios = map snd tokens

pFastRatioFractional :: String -> Bool
pFastRatioFractional input =
    L.and . map (1.0 >=) $ ratios
    where
        tokens = addTypeTokenRatio . fastTokenize $ input
        ratios = map snd tokens

-- unit: a b c d e f g h i j k l m n o p q r s t u v w x y z
assertNoDuplicatesFull :: Assertion
assertNoDuplicatesFull =
    assertBool "Type-to-token ratio is always one if there are no duplicate tokens."
               (L.all (1 ==) ratios)
    where
        input = "a b c d e f g h i j k l m n o p q r s t u v w x y z"
        ratios = map snd . addTypeTokenRatio . fullTokenize "<assertNoDuplicates>" $ input

assertNoDuplicatesFast :: Assertion
assertNoDuplicatesFast =
    assertBool "Type-to-token ratio is always one if there are no duplicate tokens."
               (L.all (1 ==) ratios)
    where
        input = "a b c d e f g h i j k l m n o p q r s t u v w x y z"
        ratios = map snd . addTypeTokenRatio . fastTokenize $ input

-- property, the ratios match
pMatchRatios :: String -> Bool
pMatchRatios input =
    L.and $ L.zipWith (==) full fast
    where full = map snd . addTypeTokenRatio . fullTokenize "<pMatchRatios>" $ input
          fast = map snd . addTypeTokenRatio . fastTokenize $ input

statsTests :: [Test]
statsTests =
    [ testGroup "fullTokenize" [ testProperty "ratio-fractional" pFullRatioFractional
                               , testCase "no-duplicates" assertNoDuplicatesFull
                               ]
    , testGroup "fastTokenizer" [ testProperty "ratio-fractional" pFastRatioFractional
                                , testCase "no-duplicates" assertNoDuplicatesFast
                                ]
    , testGroup "both-tokenizers" [ testProperty "ratios-match" pMatchRatios
                                  ]
    ]


