

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

    -- | This tokenizes a file from a source and returns a list of Tokens.
    tokenize :: String -> a -> [Token]
    tokenize = fullTokenize

    -- | This tokenizes a file from a source and returns a list of light-weight
    -- tokens, just the normalized data of the same type as the input.
    tokenize :: a -> [a]

    -- | This returns True if the input is empty.
    null :: a -> Bool

    -- | This breaks the first Char off of the input.
    uncons :: a -> Maybe (Char, a)

\end{code}

Tokenizer

This is the State monad for the tokenizer. It tracks the parser's location in
the input.

\begin{code}
type Tokenizer a = State (TokenState a) [Token]
\end{code}

TokenState tracks the tokenizer's state as it walks through the input. It keeps
track of the current index and maybe the starting index of the token it's
working on.

\begin{code}
data TokenState a = TokenState
    { cursor :: Int
    , source :: String
    , input  :: a
    }
\end{code}

fullTokenize takes an input Tokenizable instance and tokenizes it into a list
of tokens. It pulls it into a Tokenizer monad.

\begin{code}
fullTokenize :: Tokenizable a => String -> a -> [Token]
fullTokenize = evalState tokenize' . TokenState 0
    where
        tokenize' :: Tokenize Token
        tokenize' = do
            state <- get
            let (token, state') = parseToken . parseGarbage $ state in
            put state'
            case token of
                Nothing -> return []
                Just t  -> tokenize' >>= (return . (t :))

        parseGarbage :: Tokenizable a => TokenState a -> TokenState a
        parseGarbage state@(TokenState _ _ input) | null input = state
        parseGarbage state@(TokenState cursor source input)    =
            garbage cursor input
            where
                garbage :: Int -> a -> TokenState a
                garbage cursor input =
                    case uncons input of
                        Just (c, _) | isAlphaNum c ->
                            TokenState cursor source input
                        Just (_, input') ->
                            garbage (cursor + 1) input'
                        Nothing ->
                            TokenState source cursor input

        parseToken :: Tokenizable a => TokenState a -> (Maybe Token, TokenState a)
        parseToken state@(TokenState _ _ input) | null input = (Nothing, state)
        parseToken state@(TokenState cursor source input) =
            token [] 0 input
            where
                token :: String -> Int -> a -> (Maybe Token, TokenState a)
                token text tlen input =
                    case uncons input of
                        Just (c, rest) | isAlphaNum c ->
                            token (toLower c:text) (tlen+1) rest
                        Just ('\'', rest) ->
                            let (cont, clen, rest') = parseContraction rest
                                raw = (reverse text) ++ ('\'' : cont)
                                tkn = Token ((reverse text) ++ ('\'':cont)) source input
                            in
\end{code}


tokenize' handles tokenizing the input within the Tokenizer monad. garbage eats
any non-token characters, and token reads a token from the beginning of the
input and returns a Token, if it can.

\begin{code}
tokenize' :: Tokenizable a => String -> a -> [Token]

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


