{-# LANGUAGE OverloadedStrings #-}

-- | This provides an enumerator interface over the aeson JSON library.

module Text.Bakers12.JSON.Enumerators
    ( toJSONE
    ) where

import           Control.Monad.Trans (lift)
import           Data.Aeson (ToJSON(..))
import           Data.Aeson.Encode (fromValue)
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- | This takes a stream of ToJSON objects and interprets them as an array.  To
-- do this, it breaks strict monad laws by outputting a '[' before starting to
-- process the stream and by outputting a ']' when the stream is done.
toJSONE :: (ToJSON a, Monad m) => Enumeratee a T.Text m b
toJSONE = loop 0
    where
        loop n cont@(Continue k) = do
            maybeItem <- EL.head

            -- Ugly, ugly, ugly. There's probably something monadic I can do to
            -- pretty it up.
            case maybeItem of
                Just item -> do
                    let chunks = Chunks [prefix n, jsonize item]
                    next <- lift $ runIteratee $ k $ chunks
                    loop (n + 1) next
                Nothing   ->
                    case (suffix n) of
                        Just close -> do
                            next <- lift $ runIteratee $ k $ Chunks [close]
                            loop 0 next
                        Nothing    -> return cont

        loop _ step = return step

        prefix :: Int -> T.Text
        prefix 0 = "["
        prefix _ = ","

        suffix :: Int -> Maybe T.Text
        suffix 0 = Nothing
        suffix _ = Just "]"

        jsonize = TL.toStrict . TLB.toLazyText . fromValue . toJSON

