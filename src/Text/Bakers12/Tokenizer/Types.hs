
{-| Text.Bakers12.Tokenizer.Types
 - 
 - These are the types for the Baker's 12 tokenizer. The primary type is
 - Tokenizable, which includes the functions tokenize and functions to access
 - the data in the input (e.g., uncons).
 -}

module Text.Bakers12.Tokenizer.Types
    ( Token(..)
    , Tokenizable(..)
    ) where

{-| Token
 - 
 - The Token data structure contains the raw token as input, the normalized
 - token (case folded), the source of the token (filename, maybe), and the
 - token's position in the input stream.
 - 
 - The offset and length are both evaluated strictly; otherwise, if these
 - values aren't used, then their thunks accumlate and fill all available
 - space.
 -}

data Ord a => Token a = Token
    { tokenRaw     :: a          -- The raw token as it appeared in the input text.
    , tokenText    :: a          -- The normalized token.
    , tokenSource  :: String     -- The source of the token.
    , tokenOffset  :: !Int       -- The offset of the token in the input text.
    , tokenLength  :: !Int       -- The length of the token in the input text.
    }
    deriving (Show)

{-| Tokens instantiate Ord by passing it off to tokenText.
 -}

instance Ord a => Eq (Token a) where
    (==) a b = (tokenText a) == (tokenText b)

instance Ord a => Ord (Token a) where
    compare a b = compare (tokenText a) (tokenText b)

{-| Tokenizable
 - 
 - The Tokenizable type class exposes the function tokenize. It also defines
 - the interface that Tokenizable interfaced data types must define. Interfaces
 - can simply define those functions, and the tokenize functions should work
 - for free.
 -}

class Ord a => Tokenizable a where

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

