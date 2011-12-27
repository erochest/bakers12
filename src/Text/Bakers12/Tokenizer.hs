
{-| Text.Bakers12.Tokenizer
 - 
 - The Text.Bakers12.Tokenizer module defines a simple English tokenizer. It
 - breaks on whitespace, strips out punctuation (except inter-character
 - apostrophes and dashes). It maintains the token's position in the original
 - input stream (for later highlighting or other processing) and both its raw
 - and normalized forms.
 - 
 - (This hides several functions in Prelude, so you'll want to use a qualified
 - import.)
 -}

module Text.Bakers12.Tokenizer
    ( Token(..)
    , Tokenizable(..)
    , fullTokenize
    , fastTokenize
    ) where

import           Control.Monad.State
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import           Prelude hiding (dropWhile, length, null, span)
import           Text.Bakers12.Tokenizer.Types

{-| Tokenizer
 - 
 - This is the State monad for the tokenizer. It tracks the parser's location
 - in the input.
 -}

type Tokenizer a = State (TokenState a) [Token a]

{-| TokenState tracks the tokenizer's state as it walks through the input. It
 - keeps track of the current index and maybe the starting index of the token
 - it's working on.
 -}

-- | This encloses the position of the tokenizer in the input stream, the
-- input's name, and the input.
data TokenState a = TokenState Int String a

{-| These define what is and what isn't a token character.
 -}

isTokenChar :: Char -> Bool
isTokenChar c | C.isAlphaNum c    = True
              | C.isPunctuation c = True
              | otherwise         = False

{-| fullTokenize takes an input Tokenizable instance and tokenizes it into a
 - list of tokens. It pulls it into a Tokenizer monad.
 -}

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
        Just (c, _) | isTokenChar c ->
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
                Just (c, rest) | C.isPunctuation c && tlen == 0 ->
                    let (tkn, nextcur) = mkToken source [c] "" cursor
                    in  (Just tkn, TokenState nextcur source rest)
                Just (c, rest) | C.isAlphaNum c ->
                    token (c:text) (tlen+1) rest
                Just ('\'', _) ->
                    let (cont, rest') = parseContraction inp
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
                norm    = fromString . map C.toLower $ if L.null cont'
                                                       then revraw
                                                       else revraw ++ ('\'' : cont')
                rawlen  = length raw
                offset' = offset + rawlen
                tkn     = Token raw norm sourceName offset rawlen
            in  (tkn, offset')

{- | Parse a contraction, which is one or more apostrophes plus one or
 - more letters. This returns the raw contraction (with apostrophes)
 - and the rest of the input. -}
parseContraction :: Tokenizable a => a -> (String, a)
parseContraction input = contraction [] input

contraction :: Tokenizable a => String -> a -> (String, a)
contraction cont inp =
    case uncons inp of
        Just ('\'', inp') ->
            contraction ('\'':cont) inp'
        Just (c, inp') | isTokenChar c ->
            contraction (c:cont) inp'
        Just (_, inp') ->
            (reverse cont, inp')
        Nothing ->
            (reverse cont, inp)

{-| This is the fast tokenizer. It just pulls the normalized tokens off the
 - input into a list. Since it doesn't need to track the offset or anything, it
 - doesn't need to run in a State.
 -}

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
        Just (c, _) | isTokenChar c -> input
        Just (_, input')             -> fastGarbage input'
        Nothing                      -> input

fastToken :: Tokenizable a => a -> (Maybe a, a)
fastToken input =
    case (body, apos, cont) of
        (Just bc,  _     , _     ) | isSinglePunct bc -> (justString bc, input1)
        (Just b,   Just _, Just c) -> (justString . map C.toLower $ b ++ ('\'':c), input3)
        (Just b,   _,      _     ) -> (justString . map C.toLower $ b, input1)
        (Nothing,  _,      _     ) -> (Nothing, input)

    where (body, input1) = fastParseWord input []
          (apos, input2) = fastParseApos input1 []
          (cont, input3) = fastParseWord input2 []

          justString :: Tokenizable a => String -> Maybe a
          justString = Just . fromString

          isSinglePunct :: String -> Bool
          isSinglePunct [c] | C.isPunctuation c = True
          isSinglePunct _                       = False

fastParseWord :: Tokenizable a => a -> String -> (Maybe String, a)
fastParseWord inp accum =
    case uncons inp of
        Just (c, inp') | C.isPunctuation c && L.null accum ->
            (Just [c], inp')
        Just (c, inp') | C.isAlphaNum c ->
            fastParseWord inp' (c:accum)
        Just _ ->
            if L.null accum
            then (Nothing, inp)
            else (Just $ reverse accum, inp)
        Nothing | L.null accum -> (Nothing, inp)
        Nothing -> (Just $ reverse accum, inp)

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

