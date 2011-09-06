

Text.Bakers12.Tokenizer.Types

These are the types for the Baker's 12 tokenizer. The primary type is
Tokenizable, which includes the functions tokenize and functions to access the
data in the input (e.g., uncons).

\begin{code}

module Text.Bakers12.Tokenizer.Types
    ( Token(..)
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
    { raw     :: a          -- The raw token as it appeared in the input text.
    , text    :: a          -- The normalized token.
    , source  :: String     -- The source of the token.
    , offset  :: !Int       -- The offset of the token in the input text.
    , extent  :: !Int       -- The length of the token in the input text.
    }
    deriving (Show)
\end{code}


