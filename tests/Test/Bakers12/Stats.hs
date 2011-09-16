
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

summaryRatios :: String -> [Double]
summaryRatios = map snd . fst . summarize

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
    assertBool "All frequencies are two."
               (L.all (2 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = frequencies input

assertFrequencies3 :: Assertion
assertFrequencies3 =
    assertBool "All frequencies are three."
               (L.all (3 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = frequencies input

-- test cases for summaryStats
assertSumHasOutput :: Assertion
assertSumHasOutput =
    assertBool "summarize does produce output."
               (not (L.null output) || (M.null freqs))
    where
        (output, freqs) = summarize "abcdeabcedfdsakl;fdsa"

assertSumNoDups :: Assertion
assertSumNoDups =
    assertBool "Type/token is always one if there are no duplicates tokens."
               (L.all (1 ==) . summaryRatios $ input)
    where
        input = "abcdefghijklmnopqrstuvwxyz"

assertSumFreqs1 :: Assertion
assertSumFreqs1 =
    assertBool "All frequencies are one."
               (L.all (1 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyz"
        freqs = snd $ summarize input

assertSumFreqs2 :: Assertion
assertSumFreqs2 =
    assertBool "All frequencies are two."
               (L.all (2 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = snd $ summarize input

assertSumFreqs3 :: Assertion
assertSumFreqs3 =
    assertBool "All frequencies are three."
               (L.all (3 ==) . map snd . M.toList $ freqs)
    where
        input = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
        freqs = snd $ summarize input

statsTests :: [Test]
statsTests =
    [ testGroup "simple-stats" [ testProperty "ratio-fractional" pRatioFractional
                               , testCase "no-duplicates" assertNoDuplicates
                               , testCase "frequencies-1" assertFrequencies1
                               , testCase "frequencies-2" assertFrequencies2
                               , testCase "frequencies-3" assertFrequencies3
                               ]
    , testGroup "summary-stats" [ testCase "has-output" assertSumHasOutput
                                , testCase "summary-stats" assertSumNoDups
                                , testCase "summary-stats" assertSumFreqs1
                                , testCase "summary-stats" assertSumFreqs2
                                , testCase "summary-stats" assertSumFreqs3
                                ]
    ]


