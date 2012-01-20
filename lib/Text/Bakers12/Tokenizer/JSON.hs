{-# LANGUAGE OverloadedStrings #-}

-- | This just defines Token and TokenType as instances of ToJSON.

module Text.Bakers12.Tokenizer.JSON
    () where

import Data.Aeson (ToJSON(..), object, (.=))
import Text.Bakers12.Tokenizer.Types (Token(..), TokenType(..))

instance ToJSON Token where
    toJSON (Token text raw len typ src offs) =
        object [ "text"   .= text
               , "raw"    .= raw
               , "length" .= len
               , "type"   .= typ
               , "source" .= src
               , "offset" .= offs
               ]

instance ToJSON TokenType where
    toJSON = toJSON . show

