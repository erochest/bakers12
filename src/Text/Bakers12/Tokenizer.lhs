

Text.Bakers12.Tokenizer

The Text.Bakers12.Tokenizer module defines a simple English tokenizer. It
breaks on whitespace, strips out punctuation (except inter-character
apostrophes and dashes). It maintains the token's position in the original
input stream (for later highlighting or other processing) and both its raw and
normalized forms.

\begin{code}

module Text.Bakers12.Tokenizer
    ( Token(..)
    , Tokenizable(..)
    ) where

import           Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

\end{code}

Token

The Token data structure contains the raw token as input, the normalized token
(case folded), and the token's position in the input stream.

The offset and length are both evaluated strictly; otherwise, if these values
aren't used, then their thunks accumlate and fill all available space.

\begin{code}

data Token = Token
    { raw     :: T.Text     -- The raw token as it appeared in the input text.
    , text    :: T.Text     -- The normalized token.
    , source  :: String     -- The source of the token.
    , offset  :: !Int       -- The offset of the token in the input text.
    , length  :: !Int       -- The length of the token in the input text.
    }
    deriving (Show)

\end{code}

isTokenChar

This defines that is and isn't a token. This is bad, bad, bad, but it gets the
job done for what I want to do today.

\begin{code}
isTokenChar :: Char -> Bool
isTokenChar c = isAlphaNum c || c `elem` "'-"
\end{code}


Tokenizable

The Tokenizable type class exposes the function tokenize.

\begin{code}
class Tokenizable a where
    -- | This tokenizes a file from a source.
    tokenize :: String -> a -> [Token]
\end{code}

String

This tokenizes a String by converting it to a Data.Text.Text and tokenizing
that.

\begin{code}
instance (Tokenizable String) where
    tokenize src = tokenize src . T.pack
\end{code}

Data.ByteString

This tokenizes a ByteString by decoding it as UTF8, converting it to
Data.Text.Text and tokenizing that.

If the data is in another encoding, you should decode it to a Data.Text.Text
yourself and tokenize that.

\begin{code}
instance Tokenizable BS.ByteString where
    tokenize src = tokenize src . E.decodeUtf8
\end{code}

Data.ByteString.Lazy

This tokenizes a lazy ByteString just like it does a strict ByteString, but
lazily.

\begin{code}
instance Tokenizable LBS.ByteString where
    tokenize src = tokenize src . LE.decodeUtf8
\end{code}

Data.Text.Text

This tokenizes a Data.Text.Text object. This is one of the bottom turtles. It
pulls out stretches of characters.

This works using a State monad to track the parser's location in the input and
information about the current token.

Tokenizer is the State monad tokenizer.

\begin{code}
type Tokenizer a = State TokenInput [a]
\end{code}

TokenState tracks the tokenizer's state as it walks through the input. It keeps
track of the current index and maybe the starting index of the token it's
working on.

\begin{code}
data TokenState = TokenState
    { cursor :: Int
    , source :: String
    , input  :: T.Text
    }
\end{code}

tokenize' handles tokenizing the input within the Tokenizer monad. garbage eats
any non-token characters, and token reads a token from the beginning of the
input and returns a Token, if it can.

\begin{code}
tokenize' :: Tokenizer Token
tokenize' = do
    state <- get
    let (tkn, state') = token . garbage $ state
    put state'
    case tkn of
        Nothing -> return []
        Just t  -> tokenizer' >>= (return . (t :))

garbage :: TokenState -> TokenState
garbage state | (T.null . input) state = state
garbage state = garbage' (cursor state) (input state)
    where garbage' :: Int -> T.Text -> TokenState
          garbage' cur inp =
            case T.uncons inp of
                Just (c, _) | isTokenChar c ->
                    TokenState { cursor=c
                               , source=(source state)
                               , input=inp
                               }
                Just (_, inp') ->
                    garbage' (cur + 1) inp
                Nothing ->
                    TokenState { cursor=c
                               , source=(source state)
                               , input=T.empty
                               }

token :: TokenState -> (Maybe Token, TokenState)
token state | (T.null . input) state = (Nothing, (input state))
token state = token' cur cur [] (input state)
    where cur = cursor state

          token' :: Int -> Int -> String -> T.Text -> (Maybe Token, TokenState)
          token' start cursor text input =
            case T.uncons input of
                Just (c, rest) | isAlphaNum c ->
                    token' start (cursor + 1) (c : text) rest
                Just ('\'', rest) ->
                    let (cont, cursor', rest') = contraction rest in
                    let raw = T.pack $ (reverse text) ++ ('\'' : cont) in
                    ( Just Token { raw=raw
                                 , text=T.toLower raw
                                 , source=(source state)
                                 , offset=start
                                 , length=(cursor' - start) }
                    , TokenState { cursor=cursor'
                                 , source=(source state)
                                 , input=input
                                 }
                    )
                Just (_, rest) ->
                    let raw = T.pack . reverse $ text
                    ( Just Token { raw=raw
                                 , text=T.toLower raw
                                 , source=(source state)
                                 , offset=start
                                 , length=(cursor start)
                                 }
                    , TokenState { cursor=cursor + 1
                                 , source=(source state)
                                 , input=input
                                 }
                    )
                Nothing | start == cursor ->
                    (Nothing, TokenState { source=(source state), cursor=cursor+1, input=input })
                Nothing ->
                    Just Token { raw=raw
                                 , text=T.toLower raw
                                 , source=(source state)
                                 , offset=start
                                 , length=(cursor start)
                                 }
                    , TokenState { cursor=cursor + 1
                                 , source=(source state)
                                 , input=input
                                 }
                    )

\end{code}


\begin{code}
instance Tokenizable T.Text where
    tokenize text = tchar . T.span isTokenChar $ text
        where tchar (head, rest) | T.null head && T.null rest = []
                                 | T.null rest = [Token { raw=head, text=T.toLower head }]
                                 | T.null head = tokenize rest
                                 | True = Token { raw=head, text=T.toLower head } : tokenize rest
\end{code}


Data.Text.Lazy.Text

\begin{code}
instance Tokenizable LT.Text where
    tokenize text = tchar . LT.span isTokenChar $ text
        where tchar (head, rest) | LT.null head && LT.null rest = []
                                 | LT.null rest = [Token { raw=LT.toStrict head, text=normalize head }]
                                 | LT.null head = tokenize rest
                                 | True = Token { raw=LT.toStrict head, text=normalize head } : tokenize rest
              normalize = T.toLower . LT.toStrict
\end{code}

Just like Data.Text.Text, only lazier.

