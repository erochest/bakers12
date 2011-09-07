

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
    , fullTokenize
    , fastTokenize
    ) where

import           Control.Monad.State
import qualified Data.Char as C
import qualified Data.List as L
import           Prelude hiding (dropWhile, length, null, span)
import           Text.Bakers12.Tokenizer.Types

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
-- | This encloses the position of the tokenizer in the input stream, the
-- input's name, and the input.
data TokenState a = TokenState Int String a
\end{code}

fullTokenize takes an input Tokenizable instance and tokenizes it into a list
of tokens. It pulls it into a Tokenizer monad.

\begin{code}
fullTokenize :: Tokenizable a
             => String          -- The input source identifier.
             -> a               -- The input.
             -> [Token a]       -- The output list of tokens.
fullTokenize source = evalState tokenize' . TokenState 0 source

{- | Tokenize the data in the state monad -}
tokenize' :: Tokenizable a => Tokenizer a
tokenize' = do
    tokenState <- get
    let (token, tokenState') = parseToken . parseGarbage $ tokenState
    put tokenState'
    case token of
        Nothing -> return []
        Just t  -> tokenize' >>= (return . (t :))

{- | Strip any non-token characters off the front of the input. -}
parseGarbage :: Tokenizable a => TokenState a -> TokenState a
parseGarbage tokenState@(TokenState cursor source input) =
    case uncons input of
        Just (c, _) | C.isAlphaNum c ->
            tokenState
        Just (_, input') ->
            parseGarbage $ TokenState (1 + cursor) source input'
        Nothing ->
            tokenState

{- | Maybe parse a token off the front of the input, if there's
 - actually one there. -}
parseToken :: Tokenizable a => TokenState a -> (Maybe (Token a), TokenState a)
parseToken (TokenState cursor source input) =
    token [] 0 input
    where
        token :: Tokenizable a => String -> Int -> a -> (Maybe (Token a), TokenState a)
        token text tlen inp =
            case uncons inp of
                Just (c, rest) | C.isAlphaNum c ->
                    token (c:text) (tlen+1) rest
                Just ('\'', rest) ->
                    let (cont, rest') = parseContraction rest
                        (tkn, nextcur) = mkToken source text cont cursor
                    in  (Just tkn, TokenState nextcur source rest')
                Just _ ->
                    let (tkn, nextcur) = mkToken source text "" cursor
                    in  (Just tkn, TokenState nextcur source inp)
                Nothing | tlen == 0 ->
                    (Nothing, TokenState cursor source inp)
                Nothing ->
                    let (tkn, nextcur) = mkToken source text "" cursor
                    in  (Just tkn, TokenState nextcur source inp)

        {- | A factory function for tokens. -}
        mkToken :: Tokenizable a => String -> String -> String -> Int -> (Token a, Int)
        mkToken sourceName rawStart cont offset =
            let revraw  = reverse rawStart
                cont'   = L.dropWhile ('\'' ==) cont
                raw     = fromString $ revraw ++ cont
                norm    = fromString . map C.toLower $ revraw ++ ('\'' : cont')
                rawlen  = length raw
                offset' = offset + rawlen
                tkn     = Token raw norm sourceName offset rawlen
            in  (tkn, offset')

{- | Parse a contraction, which is one or more apostrophes plus one or
 - more letters. This returns the raw contraction (with apostrophes)
 - and the rest of the input.
 -
 - This also assumes that one apostrophe has already been removed from
 - the input. -}
parseContraction :: Tokenizable a => a -> (String, a)
parseContraction input = contraction ['\''] input

contraction :: Tokenizable a => String -> a -> (String, a)
contraction cont inp =
    case uncons inp of
        Just (c, inp') | C.isAlphaNum c ->
            contraction (c:cont) inp'
        Just (_, inp') ->
            (reverse cont, inp')
        Nothing ->
            (reverse cont, inp)

\end{code}

This is the fast tokenizer. It just pulls the normalized tokens off the input
into a list. Since it doesn't need to track the offset or anything, it doesn't
need to run in a State.

\begin{code}

fastTokenize :: Tokenizable a => a -> [a]
fastTokenize input =
    case currentToken of
        (Just token, input') -> (token : fastTokenize input')
        (Nothing, _)         -> []
    where
        currentToken = fastToken . fastGarbage $ input

fastGarbage :: Tokenizable a => a -> a
fastGarbage input =
    case uncons input of
        Just (c, _) | C.isAlphaNum c -> input
        Just (_, input')             -> fastGarbage input'
        Nothing                      -> input

fastToken :: Tokenizable a => a -> (Maybe a, a)
fastToken input =
    case (body, apos, cont) of
        (Just b,  Just _, Just c ) -> (Just . fromString $ b ++ ('\'':c), input3)
        (Just b,  _,      Nothing) -> (Just $ fromString b, input1)
        (Nothing, _,      _      ) -> (Nothing, input)

    where (body, input1) = fastParseWord input []
          (apos, input2) = fastParseApos input1 []
          (cont, input3) = fastParseWord input2 []

fastParseWord :: Tokenizable a => a -> String -> (Maybe String, a)
fastParseWord inp accum =
    case uncons inp of
        Just (c, inp') | C.isAlphaNum c ->
            fastParseWord inp' (c:accum)
        Just _ ->
            if L.null accum
            then (Nothing, inp)
            else (Just $ reverse accum, inp)
        Nothing -> (Nothing, inp)

fastParseApos :: Tokenizable a => a -> String -> (Maybe String, a)
fastParseApos inp accum =
    case uncons inp of
        Just ('\'', inp') ->
            fastParseApos inp' ('\'':accum)
        Just _ ->
            if L.null accum
            then (Nothing, inp)
            else (Just accum, inp)
        Nothing -> (Nothing, inp)

\end{code}

