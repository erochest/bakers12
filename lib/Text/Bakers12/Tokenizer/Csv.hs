
-- | This turns a Token into a CSVRow

module Text.Bakers12.Tokenizer.Csv
    (
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           Text.Bakers12.Csv.Types
import           Text.Bakers12.Csv.Utils
import           Text.Bakers12.Tokenizer.Types

instance ToCSV Token where
    toCSV (Token text raw len typ src offs) =
        [ text
        , raw
        , showText len
        , showText typ
        , T.pack src
        , showText offs
        ]
        where
            showText :: Show a => a -> T.Text
            showText = T.pack . show

    toCSVText (Token text raw len typ src offs) = buildRow row
        where
            row = [ escape text
                  , escape raw
                  , TBI.decimal len
                  , fromShow typ
                  , TB.fromString src
                  , TBI.decimal offs
                  ] 
            fromShow = TB.fromString . show
