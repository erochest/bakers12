
module Test.Bakers12.System.Enumerators
    ( systemEnumeratorTests
    ) where

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           Control.Monad.Trans (lift)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           System.Bakers12.Enumerators

assertPipe :: String ->
              [FilePath] ->
              [FilePath] ->
              E.Enumeratee FilePath FilePath IO [FilePath] ->
              Assertion
assertPipe msg expected input pipe = do
    output <- E.run_ (E.enumList 1 input `E.joinE` pipe
                      E.$$ EL.consume)
    let msg' = msg ++ ": " ++ show output
    assertBool msg' $ expected == output

assertRmMissingKeepExistingFiles :: Assertion
assertRmMissingKeepExistingFiles =
    assertPipe "assertRmMissingKeepExistingFiles" input input removeMissingFiles
    where input = ["bakers12.cabal", "LICENSE"]

assertRmMissingKeepExistingDirectory :: Assertion
assertRmMissingKeepExistingDirectory =
    assertPipe "assertRmMissingKeepExistingDirectory" input input removeMissingFiles
    where input = ["lib", "src", "tests"]

assertRmMissingRemoveMissingFiles :: Assertion
assertRmMissingRemoveMissingFiles =
    assertPipe "assertRmMissingRemoveMissingFiles" expected input removeMissingFiles
    where input = ["not1", "not2", "not3"]
          expected = []

assertRmMissingMixed :: Assertion
assertRmMissingMixed =
    assertPipe "assertRmMissingMixed" expected input removeMissingFiles
    where input = ["not1", "bakers12.cabal", "src", "LICENSE", "not3"]
          expected = ["bakers12.cabal", "src", "LICENSE"]

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


