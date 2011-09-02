
module Test.Bakers12.Tokenizer (tokenizerTests) where


import qualified Data.Text as T
-- import           Test.HUnit hiding (Test)
import           Test.QuickCheck
import           Test.Framework (Test)
-- import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Bakers12.Tokenizer


instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary


pRoundTrip :: T.Text -> Bool
pRoundTrip input =
    (input ==) . T.intercalate space . map raw $ tokenize input
    where space = T.singleton ' '


-- getTokenSpans
-- normalization
-- length
-- tokenization examples

tokenizerTests :: [Test]
tokenizerTests =
    [ testProperty "round trip" pRoundTrip
    ]



