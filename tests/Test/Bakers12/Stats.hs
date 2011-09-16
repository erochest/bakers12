
module Test.Bakers12.Stats (statsTests) where

import qualified Data.List as L
import qualified Data.Map as M
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Stats

ratios :: String -> [Double]
ratios = map snd . addTypeTokenRatio

-- property: ratio is always <= 1
pRatioFractional :: String -> Bool
pRatioFractional = L.and . map (1.0 >=) . ratios

-- unit: a b c d e f g h i j k l m n o p q r s t u v w x y z
assertNoDuplicates :: Assertion
assertNoDuplicates =
    assertBool "Type-to-token ratio is always one if there are no duplicate tokens."
               (L.all (1 ==) . ratios $ input)
    where
        input = "abcdefghijklmnopqrstuvwxyz"

-- test case, frequencies
assertFrequencies1 :: Assertion
assertFrequencies1 =
    assertBool "All frequencies are one."
               (L.all (1 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyz"
        freqs = frequencies input

assertFrequencies2 :: Assertion
assertFrequencies2 =
    assertBool "All frequencies are one."
               (L.all (2 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = frequencies input

assertFrequencies3 :: Assertion
assertFrequencies3 =
    assertBool "All frequencies are one."
               (L.all (3 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = frequencies input

statsTests :: [Test]
statsTests =
    [ testGroup "simple-stats" [ testProperty "ratio-fractional" pRatioFractional
                               , testCase "no-duplicates" assertNoDuplicates
                               , testCase "frequencies-1" assertFrequencies1
                               , testCase "frequencies-2" assertFrequencies2
                               , testCase "frequencies-3" assertFrequencies3
                               ]
    ]


