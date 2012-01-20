
module Test.Bakers12.Tokenizer.Minimal
    ( minimalFilterTests
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertBool)
import           Text.Bakers12.Tokenizer.Minimal (Token(..), TokenType(..), tokenize)

tokenize' :: FilePath -> T.Text -> [Token]
tokenize' source input =
    case tokenize source input of
        Right tokens -> tokens
        Left  err    -> []

assertTokenizes :: String -> String -> [String] -> Assertion
assertTokenizes msg input expected = assertBool msg' $ expected == output
    where
        msg'   = msg ++ ": " ++ show output
        output = map (T.unpack . tokenText) . tokenize' msg $ T.pack input

assertAll :: String -> String -> (Token -> Bool) -> Assertion
assertAll msg input p = assertBool msg' all
    where
        msg'   = msg ++ ": " ++ show output
        output = tokenize' msg $ T.pack input
        all    = L.all p output


assertRemovesWhitespace :: Assertion
assertRemovesWhitespace = assertAll "assertRemovesWhitespace" input pred
    where
        input = "some text here. i really don't care what."
        pred  = (SeparatorToken /=) . tokenType


minimalFilterTests :: [Test]
minimalFilterTests =
    [ testGroup "whitespace" [ testCase "missing" assertRemovesWhitespace
                             ]
    ]

