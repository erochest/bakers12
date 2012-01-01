
module Test.Bakers12.System.Enumerators
    ( systemEnumeratorTests
    ) where

import           Control.Applicative
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           Control.Monad.Trans (lift)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           System.Bakers12.Enumerators
import           System.Directory
import           System.FilePath

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
    where input    = ["not1", "not2", "not3"]
          expected = []

assertRmMissingMixed :: Assertion
assertRmMissingMixed =
    assertPipe "assertRmMissingMixed" expected input removeMissingFiles
    where input    = ["not1", "bakers12.cabal", "src", "LICENSE", "not3"]
          expected = ["bakers12.cabal", "src", "LICENSE"]

assertExpDirFiles :: Assertion
assertExpDirFiles =
    assertPipe "assertExpDirFiles" expected input expandDirectories
    where input    = ["bakers12.cabal", "LICENSE"]
          expected = input

assertExpDirMissing :: Assertion
assertExpDirMissing =
    assertPipe "assertExpDirMissing" expected input expandDirectories
    where input    = ["doesnotexist"]
          expected = []

assertExpDirExpandShallow :: Assertion
assertExpDirExpandShallow = do
    expected <- map ("bin" </>) . filter (`notElem` [".", ".."])
                <$> getDirectoryContents "bin"
    assertPipe "assertExpDirExpandShallow" expected input expandDirectories
    where input = ["bin"]

-- TODO: This will break at every opportunity!
assertExpDirExpandDeep :: Assertion
assertExpDirExpandDeep =
    assertPipe "assertExpDirExpandDeep" expected input expandDirectories
    where input    = ["tests"]
          expected = [ "tests/TestBakers12.hs"
                     , "tests/Test/Bakers12/Tokenizer.hs"
                     , "tests/Test/Bakers12/Tokenizer/PennTreebank.hs"
                     , "tests/Test/Bakers12/System/Enumerators.hs"
                     ]

assertExpDirExpandMixed :: Assertion
assertExpDirExpandMixed = do
    expected <- ("bakers12.cabal" :)
                <$> map ("bin" </>) . filter (`notElem` [".", ".."])
                <$> getDirectoryContents "bin"
    assertPipe "assertExpDirExpandMixed" expected input expandDirectories
    where input    = ["doesnotexist", "bakers12.cabal", "bin"]

assertEnumDirectory :: String -> FilePath -> [FilePath] -> Assertion
assertEnumDirectory msg dirname expected = do
    output <- E.run_ (enumDirectory dirname E.$$ EL.consume)
    let msg' = msg ++ ": " ++ show output
    assertBool msg' $ expected == output

assertEnDirShallow :: Assertion
assertEnDirShallow = do
    expected <- map ("bin" </>) . filter (`notElem` [".", ".."])
                <$> getDirectoryContents "bin"
    assertEnumDirectory "assertEnDirShallow" "bin" expected

-- TODO: This will break at every opportunity!
assertEnDirDeep :: Assertion
assertEnDirDeep =
    assertEnumDirectory "assertEnDirDeep" "tests" expected
    where input    = ["tests"]
          expected = [ "tests/TestBakers12.hs"
                     , "tests/Test/Bakers12/Tokenizer.hs"
                     , "tests/Test/Bakers12/Tokenizer/PennTreebank.hs"
                     , "tests/Test/Bakers12/System/Enumerators.hs"
                     ]


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
                                     , testCase "mixed" assertExpDirExpandMixed
                                     ]
    , testGroup "enumDirectory"      [ testCase "enumerate a shallow directory" assertEnDirShallow
                                     , testCase "enumerate a deep directory" assertEnDirDeep
                                     ]
    ]


