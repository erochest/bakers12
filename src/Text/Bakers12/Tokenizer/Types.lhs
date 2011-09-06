

Text.Bakers12.Tokenizer.Types

These are the types for the Baker's 12 tokenizer. The primary type is
Tokenizable, which includes the functions tokenize and functions to access the
data in the input (e.g., uncons).

\begin{code}

module Text.Bakers12.Tokenizer.Types
    ( Token(..)
    , Tokenizable(..)
    ) where

\end{code}

Token

The Token data structure contains the raw token as input, the normalized token
(case folded), the source of the token (filename, maybe), and the token's
position in the input stream.

The offset and length are both evaluated strictly; otherwise, if these values
aren't used, then their thunks accumlate and fill all available space.

\begin{code}
data Token a = Token
    { tokenRaw     :: a          -- The raw token as it appeared in the input text.
    , tokenText    :: a          -- The normalized token.
    , tokenSource  :: String     -- The source of the token.
    , tokenOffset  :: !Int       -- The offset of the token in the input text.
    , tokenLength  :: !Int       -- The length of the token in the input text.
    }
    deriving (Show)
\end{code}

Tokenizable

The Tokenizable type class exposes the function tokenize. It also defines the
interface that Tokenizable interfaced data types must define. Interfaces can
simply define those functions, and the tokenize functions should work for free.

\begin{code}
class Tokenizable a where

    -- | This returns True if the input is empty.
    null :: a -> Bool

    -- | This breaks the first Char off of the input.
    uncons :: a -> Maybe (Char, a)

    -- | This converts a string to the tokenizable type.
    fromString :: String -> a

    -- | This converts a tokenizable type to a string (for brief periods).
    toString :: a -> String

    -- | This returns the length of a tokenizable.
    length :: a -> Int

    -- | This splits the input into two parts according to a predicate.
    span :: (Char -> Bool) -> a -> (a, a)

    -- | This drops everything that matches a predicate from the beginning of
    -- the input.
    dropWhile :: (Char -> Bool) -> a -> a

    -- | This lower cases it.
    toLower :: a -> a

\end{code}

