
{-|
 - Module      : Text.Bakers12.Tokenizer
 - License     : Apache 2.0
 - Maintainer  : erochest@gmail.com
 - Portability : GHC
 - 
 - This implements the tokenizer. It returns all tokens, without throwing any
 - text away. That way, the unprocessed token stream can be used to re-create
 - the input.
 - 
 - The processing uses the
 - [enumerator](http://hackage.haskell.org/package/enumerator) library to
 - produce a stream of tokens from a stream of input.
 - 
 - The `tokenize` and `tokenizeFile` functions hide this by accumulating the
 - output tokens into a list and returning that.
 -}

module Text.Bakers12.Tokenizer
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , tokenizeStream
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- * Token Type

-- | This contains data from one token instance.
data Token = Token
    { tokenText   :: T.Text         -- ^ The normalized token text.
    , tokenRaw    :: T.Text         -- ^ The raw token text.
    , tokenLength :: Int            -- ^ The length of the raw token.
    , tokenType   :: TokenType      -- ^ The type of data contained in the
                                    -- token.
    }
    deriving (Eq, Show)

-- | This has the types of information that a token can contain.
data TokenType =
      AlphaToken                    -- ^ Unicode alphabetic characters.
    | NumberToken                   -- ^ Unicode numeric characters.
    | SeparatorToken                -- ^ Unicode space or Unicode separator
                                    -- character.
    | PunctuationToken              -- ^ One Unicode punctuation character.
    | SymbolToken                   -- ^ One Unicode symbol character.
    | MarkToken                     -- ^ One Unicode mark character.
    | UnknownToken                  -- ^ None of the categories above.
    deriving (Eq, Show)

-- | This reads text from an instance of Data.Text.Text and returns a list of
-- Token instances.
tokenize :: T.Text -> Either SomeException [Token]
tokenize input = E.runLists [[input]] (tokenizeStream E.=$ EL.consume)

-- | This reads the input from a file and returns a list of Token instances.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (tokenizeFileStream inputFile E.$$ EL.consume)

-- | This creates an Enumerator that reads from a file and produces Tokens.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile = ET.enumFile inputFile E.$= tokenizeStream

-- | This is an Enumeratee that takes a stream of Char and transforms it into a
-- stream of Token.
tokenizeStream :: Monad m => E.Enumeratee T.Text Token m b
tokenizeStream cont@(E.Continue k) = do
    maybeC <- ET.head
    case maybeC of
        Just c  -> do
            token <- tokenize' c
            next  <- lift $ E.runIteratee $ k $ E.Chunks [token]
            tokenizeStream next
        Nothing -> return cont
tokenizeStream step = return step

-- | This takes a character and dispatches the handle the tokenizing the rest
-- of the token from it.
tokenize' :: Monad m => Char -> E.Iteratee T.Text m Token
tokenize' c | C.isAlpha c       = tokenFromTaken AlphaToken c C.isAlpha
tokenize' c | C.isNumber c      = tokenFromTaken NumberToken c C.isNumber
tokenize' c | isSeparator c     = tokenFromTaken SeparatorToken c isSeparator
tokenize' c | C.isPunctuation c = return . makeToken PunctuationToken $ T.singleton c
tokenize' c | C.isSymbol c      = return . makeToken SymbolToken $ T.singleton c
tokenize' c | C.isMark c        = return . makeToken MarkToken $ T.singleton c
tokenize' c | otherwise         = return . makeToken UnknownToken $ T.singleton c

-- | This is an augmented separator predicate that also tests for spaces.
isSeparator :: Char -> Bool
isSeparator c = C.isSpace c || C.isSeparator c

-- | This runs takeWhile with the predicate, conses the initial element to the
-- front, and creates a Token of the given type.
tokenFromTaken :: Monad m =>
                  TokenType -> Char -> (Char -> Bool) -> E.Iteratee T.Text m Token
tokenFromTaken tType initial predicate =
    ET.takeWhile predicate >>=
    return . makeToken tType . LT.toStrict . LT.cons initial

-- | In the context of an Enumerator, this takes a [Char] list and returns a
-- Token.
makeToken :: TokenType -> T.Text -> Token
makeToken tType raw = Token normalized raw rawLength tType
    where
        normalized = normalizeToken raw
        rawLength  = T.length raw

-- | This takes a raw token Text and returns a normalized version. Currently,
-- this just lower-cases everything.
normalizeToken :: T.Text -> T.Text
normalizeToken = T.map C.toLower

