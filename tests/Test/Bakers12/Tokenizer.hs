
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


-- Full Tokenizer Tests:
-- property : normalized (toLower)
-- property : length raw == tokenLength
-- property : tokenOffset is monotonically increasing
-- unit     : tokenized correctly
-- unit     : tokenized numbers
-- unit     : tokenized contractions
-- unit     : not tokenized leading-, trailing-apostrophes

pRoundTrip :: String -> Bool
pRoundTrip input =
    (input ==) . L.intercalate " " . map tokenRaw $ fullTokenize "<test>" input

-- Fast Tokenizer Tests:
-- property : toLower
-- unit     : tokenized correctly
-- unit     : tokenized numbers
-- unit     : tokenized contractions
-- unit     : not tokenized leading-, trailing-apostrophes

-- Both:
-- property : normalized output is the same for both


tokenizerTests :: [Test]
tokenizerTests =
    [ testProperty "round trip" pRoundTrip
    ]



