

Text.Bakers12.Tokenizer

The Text.Bakers12.Tokenizer module defines a simple English tokenizer. It
breaks on whitespace, strips out punctuation (except inter-character
apostrophes and dashes). It maintains the token's position in the original
input stream (for later highlighting or other processing) and both its raw and
normalized forms.

(This hides several functions in Prelude, so you'll want to use a qualified
import.)

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
import           Prelude hiding (dropWhile, length, null, span)
import           Text.Bakers12.Tokenizer.Types

\end{code}

Tokenizable

The Tokenizable type class exposes the function tokenize. It also defines the
interface that Tokenizable interfaced data types must define. Interfaces can
simply define those functions, and the tokenize functions should work for free.

\begin{code}
class Tokenizable a where

    -- | This tokenizes a file from a source and returns a list of Tokens.
    fullTokenize :: String -> a -> [Token a]
    fullTokenize = fullTokenize'

    -- | This tokenizes a file from a source and returns a list of light-weight
    -- tokens, just the normalized data of the same type as the input.
    tokenize :: a -> [a]
    tokenize = fastTokenize'

    -- | This returns True if the input is empty.
    null :: a -> Bool

    -- | This breaks the first Char off of the input.
    uncons :: a -> Maybe (Char, a)

    -- | This converts a string to the tokenizable type.
    fromString :: String -> a

    -- | This returns the length of a tokenizable.
    length :: a -> Int

    -- | This splits the input into two parts according to a predicate.
    span :: (Char -> Bool) -> (a, a)

    -- | This drops everything that matches a predicate from the beginning of
    -- the input.
    dropWhile :: (Char -> Bool) -> a -> a

\end{code}

Tokenizer

This is the State monad for the tokenizer. It tracks the parser's location in
the input.

\begin{code}
type Tokenizer a = State (TokenState a) [Token a]
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
fullTokenize' :: Tokenizable a => String -> a -> [Token a]
fullTokenize' source = evalState tokenize' . TokenState 0 source
    where
        {- | Tokenize the data in the state monad -}
        tokenize' :: Tokenizer a
        tokenize' = do
            state <- get
            (token, state') <- return . parseToken . parseGarbage $ state
            put state'
            case token of
                Nothing -> return []
                Just t  -> tokenize' >>= (return . (t :))

        {- | Strip any non-token characters off the front of the input. -}
        parseGarbage :: TokenState a -> TokenState a
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

        {- | Maybe parse a token off the front of the input, if there's
         - actually one there. -}
        parseToken :: TokenState a -> (Maybe (Token a), TokenState a)
        parseToken state@(TokenState _ _ input) | null input = (Nothing, state)
        parseToken state@(TokenState cursor source input)    =
            token [] 0 input
            where
                token :: String -> Int -> a -> (Maybe (Token a), TokenState a)
                token text tlen input =
                    case uncons input of
                        Just (c, rest) | isAlphaNum c ->
                            token (c:text) (tlen+1) rest
                        Just ('\'', rest) ->
                            let (cont, clen, rest') = parseContraction rest
                                (tkn, nextcur) = mkToken source text cont cursor
                            in  (Just tkn, TokenState nextcur source rest')
                        Just (_, rest) ->
                            let (tkn, nextcur) = mkToken source text "" cursor
                            in  (Just tkn, TokenState nextcur source input)
                        Nothing | tlen == 0 ->
                            (Nothing, TokenState cursor source input)
                        Nothing ->
                            let (tkn, nextcur) = mkToken source text "" cursor
                            in  (Just tkn, TokenState nextcur source input)

        {- | Parse a contraction, which is one or more apostrophes plus one or
         - more letters. This returns the raw contraction (with apostrophes),
         - the length of the raw contraction, and the rest of the input.
         -
         - This also assumes that one apostrophe has already been removed from
         - the input. -}
        parseContraction :: a -> (String, String, Int, a)
        parseContraction input = contraction ['\''] 0 input
            where
                contraction :: String -> Int -> a -> (String, String, Int, a)
                contraction cont clen inp =
                    case uncons inp of
                        Just (c, inp') | isAlphaNum c ->
                            contraction (c:cont) (clen+1) inp'
                        Just (_, inp') ->
                            (reverse cont, clen, inp')
                        Nothing ->
                            (reverse cont, clen, inp)

        {- | A factory function for tokens. -}
        mkToken :: String -> String -> String -> Int -> (Token a, Int)
        mkToken source rawStart cont offset =
            let revraw  = reverse rawStart
                cont'   = dropWhile ('\'' ==) cont
                raw     = fromString $ revraw ++ cont
                norm    = fromString . toLower $ revraw ++ ('\'' : cont')
                rawlen  = length raw
                offset' = offset + rawlen
                tkn     = Token raw norm source offset rawlen
            in  (tkn, offset')

\end{code}

This is the fast tokenizer. It just pulls the normalized tokens off the input
into a list. Since it doesn't need to track the offset or anything, it doesn't
need to run in a State.

\begin{code}

fastTokenize' :: Tokenizable a => a -> [a]
fastTokenize' input =
    case currentToken of
        (Just token, input') -> (token : fastTokenize' input')
        (Nothing, _)         -> []
    where
        currentToken = parseToken . parseGarbage $ input

        parseGarbage :: Tokenizable a => a -> a
        parseGarbage input =
            case uncons input of
                Just (c, _) | isAlphaNum c -> input
                Just (_, input')           -> parseGarbage input'
                Nothing                    -> input

        parseToken :: Tokenizable a => a -> (Maybe a, a)
        parseToken input =
            let (prefix, input1) = span isAlphaNum input
                (cont, input2)   = span ('\'' ==)  input1
            in  if null cont
                then (Just $ toLower prefix, input1)
                else let (suffix, input3) = span isAlphaNum input2
                         token = prefix ++ ('\'' : suffix)
                     in  (Just $ toLower token, input3)

\end{code}

