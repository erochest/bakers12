
-- | This allows you to generate CSV from a number of data types. The
-- conversion functions work in the context of a Data.Text.Lazy.Builder.

module Text.Bakers12.Csv
    ( CSVRow
    , ToCSV(..)
    , toCSVE
    , escape
    ) where

import           Text.Bakers12.Csv.Enumerators
import           Text.Bakers12.Csv.Types (CSVRow, ToCSV(..))
import           Text.Bakers12.Csv.Utils

