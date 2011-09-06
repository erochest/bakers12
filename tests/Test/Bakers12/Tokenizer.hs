
module Test.Bakers12.Tokenizer (tokenizerTests) where


import qualified Data.List as L
-- import qualified Data.Text as T
-- import           Test.HUnit hiding (Test)
-- import           Test.QuickCheck
import           Test.Framework (Test)
-- import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Tokenizer
import           Text.Bakers12.Tokenizer.String ()


pRoundTrip :: String -> Bool
pRoundTrip input =
    (input ==) . L.intercalate " " . map tokenRaw $ fullTokenize "<test>" input


-- getTokenSpans
-- normalization
-- length
-- tokenization examples

tokenizerTests :: [Test]
tokenizerTests =
    [ testProperty "round trip" pRoundTrip
    ]



