
module Test.Bakers12.ANC (ancTests) where


import qualified Data.List as L
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Bakers12.Tokenizer
import           Text.Bakers12.Tokenizer.String ()
import qualified Text.Bakers12.Tokenizer.Xml as X


assertComma :: Assertion
assertComma = do
    tokens <- X.fullTokenize "assert-comma" "id" input
    let actual = map tokenText tokens
        msg    = "assert comma => " ++ show actual
    assertBool msg . L.and $ L.zipWith (==) expected actual
    where
        input = "<s id='p1s1'>In the late 1940s, Bond Stores, the largest \
                \men's clothing chain at the time, created a sensation in \
                \New York City by offering a wide selection of suits with\
                \ two pairs of pants instead of one, reintroducing a level \
                \of product choice not seen since before the war.</s>"
        expected = [ "in"
                   , "the"
                   , "late"
                   , "1940s"
                   , ","
                   , "bond"
                   , "stores"       -- store
                   , ","
                   , "the"
                   , "largest"      -- large
                   , "men"          -- man
                   , "'s"
                   , "clothing"
                   , "chain"
                   , "at"
                   , "the"
                   , "time"
                   , ","
                   , "created"
                   , "a"
                   , "sensation"
                   , "in"
                   , "new"
                   , "york"
                   , "city"
                   , "by"
                   , "offering"
                   , "a"
                   , "wide"
                   , "selection"
                   , "of"
                   , "suits"        -- suits
                   , "with"
                   , "two"
                   , "pairs"        -- pair
                   , "of"
                   , "pants"
                   , "instead"
                   , "of"
                   , "one"
                   , ","
                   , "reintroducing"
                   , "a"
                   , "level"
                   , "of"
                   , "product"
                   , "choice"
                   , "not"
                   , "see"
                   , "since"
                   , "before"
                   , "the"
                   , "war"
                   , "."
                   ]

ancTests :: [Test]
ancTests =
    [ testGroup "punctuation" [ testCase "comma" assertComma
                              ]
    ]

