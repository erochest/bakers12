{-# LANGUAGE FlexibleInstances #-}

-- | This handles the types required for Text.Bakers12.Csv conversion.

module Text.Bakers12.Csv.Types
    ( CSVRow
    , ToCSV(..)
    ) where

import qualified Data.Text as T
import           Text.Bakers12.Csv.Utils

-- | This is the type of a row of CSV data.
type CSVRow = [T.Text]

-- | A type that can be converted to CSV.
class ToCSV a where

    -- | This handles actually converting a list of CSV 
    toCSV :: a -> CSVRow

    -- | This converts it to a Text. This has a default implementation, but you
    -- can speed things up by defining it yourself (and not escaping every
    -- field, for instance).
    toCSVText :: a -> T.Text
    toCSVText item = buildRow . map escape $ toCSV item

instance ToCSV [T.Text] where
    toCSV = id

