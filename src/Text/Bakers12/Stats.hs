
{-| - Text.Bakers12.Stats
 - 
 - This contains utility functions and things that just don't seem to fit anywhere
 - else.
 -}

module Text.Bakers12.Stats
    ( addTypeTokenRatio
    , frequencies
    , summarize
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

{-| This is a utility to handle type conversions and compute the ratio.
 -}

ratio :: Int -> Int -> Double
ratio typeCount tokenCount =
    (fromIntegral typeCount) / (fromIntegral tokenCount)

{-| This is a type for holding the accumulated data for computing the running
 - type-to-token ratios.
 -}

data RatioAccum a = RatioAccum (S.Set a) Int

{-| step is where all the action happens. It keeps a set of and a count of the
 - tokens seen, and it computes the ratio from the size of the set and the
 - count.
 -}

step :: Ord a => RatioAccum a -> a -> (RatioAccum a, (a, Double))
step (RatioAccum types tokenCount) token = (accum', (token, ratio'))
    where
        types'      = S.insert token types
        tokenCount' = tokenCount + 1
        accum'      = RatioAccum types' tokenCount'
        ratio'      = ratio (S.size types') tokenCount'

{-| This takes tokens and decorates the stream with the running type-to-token
 - ratio.
 -}

addTypeTokenRatio :: Ord a => [a] -> [(a, Double)]
addTypeTokenRatio tokens =
    snd . L.mapAccumL step (RatioAccum S.empty 0) $ tokens

{-| The frequencies code takes a list of Ords and returns a Map mapping the
 - items in the list to their frequencies in it.
 -}

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = L.foldl' freq' M.empty

freq' :: Ord a => M.Map a Int -> a -> M.Map a Int
freq' m k = M.insertWith' (+) k 1 m

{-| summarize is a combination of addTypeTokenRatio and frequencies. Because it
 - only traverses the input once, it is more efficient.
 -}

data SumAccum a = SumAccum (RatioAccum a) (M.Map a Int)

summarize :: Ord a => [a] -> ([(a, Double)], M.Map a Int)
summarize input =
    let (SumAccum _ freqs, output) = L.mapAccumL sumStep start $ input
    in  (output, freqs)
    where
        start :: SumAccum a
        start = SumAccum (RatioAccum S.empty 0) M.empty

sumStep :: Ord a => SumAccum a -> a -> (SumAccum a, (a, Double))
sumStep (SumAccum accum freqMap) item = (SumAccum accum' freqOut, output)
    where
        (accum', output) = step accum item
        freqOut = freq' freqMap item

