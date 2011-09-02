
module Main where

import Test.HUnit (Assertion, assertBool, assertFailure)
import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)


-- One QuickCheck test.
pStringReverse :: String -> Bool
pStringReverse xs =
    xs == (reverse . reverse $ xs)


-- One HUnit test.
assertAlwaysTrue :: Assertion
assertAlwaysTrue =
    assertBool "I'm always true!" True

assertAlwaysFails :: Assertion
assertAlwaysFails =
    assertFailure "I'm a failure. Boo-hoo."


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "properties" [ testProperty "string/reverse" pStringReverse ]
    , testGroup "units"
      [ testCase "always-true" assertAlwaysTrue
      , testCase "always-fails" assertAlwaysFails
      ]
    ]

