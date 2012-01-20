
-- | This defines some enumerators to take a stream of ToCSV instances and
-- output a ByteString of CSV.

module Text.Bakers12.Csv.Enumerators
    ( toCSVE
    ) where

import           Control.Monad.Trans (lift)
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import           Text.Bakers12.Csv.Types

-- | This takes a stream of ToCSV and outputs the Text of the CSV.
toCSVE :: (ToCSV a, Monad m) => Enumeratee a T.Text m b
toCSVE cont@(Continue k) = do
    maybeItem <- EL.head
    case maybeItem of
        Just item -> do
            next <- lift $ runIteratee $ k $ Chunks [toCSVText item]
            toCSVE next
        Nothing -> return cont
toCSVE step = return step

