
module Test.Bakers12.System.Enumerators
    ( systemEnumeratorTests
    ) where

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           System.Bakers12.Enumerators

assertRmMissingKeepExistingFiles :: Assertion
assertRmMissingKeepExistingFiles = do
    output <- E.run_ (E.enumList 1 input `E.joinE` removeMissingFiles
                      E.$$ EL.consume)
    assertBool ("assertRmMissingKeepExistingFiles: " ++ show output) $ input == output
    where input = ["bakers12.cabal", "LICENSE"]

assertRmMissingKeepExistingDirectory :: Assertion
assertRmMissingKeepExistingDirectory =
    assertBool "assertRmMissingKeepExistingDirectory" False

assertRmMissingRemoveMissingFiles :: Assertion
assertRmMissingRemoveMissingFiles =
    assertBool "assertRmMissingRemoveMissingFiles" False

assertRmMissingMixed :: Assertion
assertRmMissingMixed =
    assertBool "assertRmMissingMixed" False

assertExpDirFiles :: Assertion
assertExpDirFiles =
    assertBool "assertExpDirFiles" False

assertExpDirMissing :: Assertion
assertExpDirMissing =
    assertBool "assertExpDirMissing" False

assertExpDirExpandShallow :: Assertion
assertExpDirExpandShallow =
    assertBool "assertExpDirExpandShallow" False

assertExpDirExpandDeep :: Assertion
assertExpDirExpandDeep =
    assertBool "assertExpDirExpandDeep" False

assertEnDirShallow :: Assertion
assertEnDirShallow =
    assertBool "assertEnDirShallow" False

assertEnDirDeep :: Assertion
assertEnDirDeep =
    assertBool "assertEnDirDeep" False


systemEnumeratorTests :: [Test]
systemEnumeratorTests =
    [ testGroup "removeMissingFiles" [ testCase "file exists" assertRmMissingKeepExistingFiles
                                     , testCase "directory exists" assertRmMissingKeepExistingDirectory
                                     , testCase "file missing" assertRmMissingRemoveMissingFiles
                                     , testCase "mixed" assertRmMissingMixed
                                     ]
    , testGroup "expandDirectories"  [ testCase "pass through files" assertExpDirFiles
                                     , testCase "skip missing" assertExpDirMissing
                                     , testCase "expand shallow directory" assertExpDirExpandShallow
                                     , testCase "expand deep directory" assertExpDirExpandDeep
                                     ]
    , testGroup "enumDirectory"      [ testCase "enumerate a shallow directory" assertEnDirShallow
                                     , testCase "enumerate a deep directory" assertEnDirDeep
                                     ]
    ]


