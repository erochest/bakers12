
Text.Bakers12.Utils

This contains utility functions and things that just don't seem to fit anywhere
else.

\begin{code}
module Text.Bakers12.Utils
    ( addTypeTokenRatio --addFullTypeTokenRatio
    -- , addFastTypeTokenRatio
    ) where

import           Text.Bakers12.Tokenizer.Types
import qualified Data.List as L
import qualified Data.Set as S
\end{code}

This is a utility to handle type conversions and compute the ratio.

\begin{code}
ratio :: Int -> Int -> Double
ratio typeCount tokenCount =
    (fromIntegral typeCount) / (fromIntegral tokenCount)
\end{code}

This is a type for holding the accumulated data for computing the running
type-to-token ratios.

\begin{code}
data RatioAccum a = RatioAccum (S.Set a) Int
\end{code}

step is where all the action happens. It keeps a set of and a count of the
tokens seen, and it computes the ratio from the size of the set and the count.

\begin{code}
step :: Ord a => RatioAccum a -> a -> (RatioAccum a, (a, Double))
step (RatioAccum types tokenCount) token = (accum', (token, ratio'))
    where
        types'      = S.insert token types
        tokenCount' = tokenCount + 1
        accum'      = RatioAccum types' tokenCount'
        ratio'      = ratio (S.size types') tokenCount'
\end{code}

This takes tokens and decorates the stream with the running type-to-token
ratio.

\begin{code}
addTypeTokenRatio :: Ord a => [a] -> [(a, Double)]
addTypeTokenRatio tokens =
    snd . L.mapAccumL step (RatioAccum S.empty 0) $ tokens
\end{code}

