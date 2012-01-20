
-- | Utilities for working with CSV data.

module Text.Bakers12.Csv.Utils
    ( escape
    , buildRow
    ) where

import qualified Data.Char as C
import           Data.Monoid (mappend)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

-- | This handles escaping values.
escape :: T.Text -> TB.Builder
escape input = if T.any (not . C.isAlphaNum) input
               then quote `mappend` (TB.fromText escaped `mappend` quote)
               else TB.fromText input
    where
        quote   = TB.singleton '"'
        escaped = T.replace (T.singleton '"') (T.pack "\"\"") input

-- | This takes a list of Builders, one for each field, and assembles them into
-- a CSV row. Basically, it just adds commas and a newline.
buildRow :: [TB.Builder] -> T.Text
buildRow row = TL.toStrict $ TB.toLazyText line
    where 
        comma = TB.singleton ','
        nl    = TB.singleton '\n'
        line  = foldr mappend nl . L.intersperse comma $ row

