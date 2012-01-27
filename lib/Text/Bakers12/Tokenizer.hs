
-- This implements the tokenizer. It returns all tokens, without throwing any
-- text away. That way, the unprocessed token stream can be used to re-create
-- the input.
-- 
-- The processing uses the
-- [enumerator](http://hackage.haskell.org/package/enumerator) library to
-- produce a stream of tokens from a stream of input or to convert an input
-- stream into a stream of `Token` instances.
-- 
-- The `tokenize` and `tokenizeFile` functions hide this by accumulating the
-- output tokens into a list and returning that.

module Text.Bakers12.Tokenizer
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , tokenizeStream
    , tokenizeE
    ) where

import           Control.Exception (SomeException)
import           Control.Monad (liftM)
import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
-- [Text.Bakers12.Tokenizer.Types](Tokenizer/Types.html)<br />
import           Text.Bakers12.Tokenizer.Types

-- This reads text from an instance of Data.Text.Text and returns a list of
-- Token instances.
--
-- `Either SomeException` is a standard way to define a computation that may
-- return something (in this case, a list of tokens) or may have an error.
tokenize :: FilePath -> T.Text -> Either SomeException [Token]
tokenize source input =
    E.runLists [[input]] (tokenizeStream source 0 E.=$ EL.consume)

-- This reads the input from a file and returns a list of Token instances.
--
-- Because this has to read from the file system, it has to execute in the
-- context of the `IO` monad. Otherwise, this is the same as `tokenize`.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (tokenizeFileStream inputFile E.$$ EL.consume)

-- This creates an `Enumerator` that reads from a file and produces `Token`
-- instances.
--
-- Enumerators produce data for an enumerator processing pipeline.
--
-- This assumes that the files are UTF8.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile =
    EB.enumFile inputFile E.$=
    ET.decode ET.utf8 E.$=
    tokenizeStream inputFile 0

-- This is an `Enumeratee` that takes `FilePath` and returns a `Enumerator` of
-- `Token` instances.
--
-- `Enumeratees` are links in the pipeline between an `Enumerator` and an
-- `Iteratee`. They take input coming in and filter or transform it and pass it
-- on. Because they're in the middle, they have to pay more attention to the
-- enumerator API. I'll take some time to explain how that works as we
-- encounter it.
tokenizeE :: E.Enumeratee FilePath Token IO b
-- First, one of the values that an enumeratee can accept is a continuation.
-- This means that there is another chunk of data available.
tokenizeE cont@(E.Continue _) = do
    -- This returns the first item waiting on the input stream. `Continue`
    -- means that there is more data to come, but we may not have it right now
    -- (say for networking).  So `EL.head` may return data, or it may not. In
    -- Haskell, this means a `Maybe` data type.
    maybeFP <- EL.head
    case maybeFP of
        -- If there is data right now, it's a file path. We just pass it to
        -- `tokenizeFileStream`, which returns a bunch of tokens, and then
        -- loop.
        Just filePath -> tokenizeFileStream filePath cont E.>>== tokenizeE
        -- If there is no data at the moment, just continue waiting.
        Nothing       -> return cont
-- Other enumeratee inputs mean other things (no more data, end of stream,
-- etc.), but we don't care about that.
tokenizeE step = return step

-- *(After explaining this, there's an obvious way to cut down on the
-- boilerplate for this, at least for this application. It won't happen today,
-- however.)*

-- This is an Enumeratee that takes a stream of Char and transforms it into a
-- stream of Token.
--
-- `FilePath` and `Integer` are the current state of the tokenizer. This should
-- probably be wrapped in a `State` monad and stacked on top of this, but for
-- the moment, this is simpler.
tokenizeStream :: Monad m =>
                  FilePath -> Integer -> E.Enumeratee T.Text Token m b
tokenizeStream source offset cont@(E.Continue k) = do
    -- This looks at the first character from the input ...
    maybeC <- ET.head
    case maybeC of
        Just c  -> do
            -- ... and dispatches to `tokenize'` based on the first character.
            -- It passes the token into the output stream and loops to take
            -- care of the next token.
            token <- tokenize' source offset c
            next  <- lift $ E.runIteratee $ k $ E.Chunks [token]
            tokenizeStream source (offset + fromIntegral (tokenLength token)) next
        Nothing -> return cont
tokenizeStream _ _ step = return step

-- This takes a character and dispatches the handle the tokenizing the rest
-- of the token from it.
tokenize' :: Monad m => FilePath -> Integer -> Char -> E.Iteratee T.Text m Token
tokenize' source offset c
    -- Tokens that contain alphanumeric or spaces/separators return a span of
    -- tokens of the same type.
    | C.isAlpha c       = tokenFromTaken source offset AlphaToken c C.isAlpha
    | C.isNumber c      = tokenFromTaken source offset NumberToken c C.isNumber
    | isSeparator c     = tokenFromTaken source offset SeparatorToken c isSeparator

    -- Punctuation, symbols, marks, and what-have-yous are single-character
    -- tokens.
    | C.isPunctuation c = return . makeToken source offset PunctuationToken $ T.singleton c
    | C.isSymbol c      = return . makeToken source offset SymbolToken $ T.singleton c
    | C.isMark c        = return . makeToken source offset MarkToken $ T.singleton c
    | otherwise         = return . makeToken source offset UnknownToken $ T.singleton c

-- This is an augmented separator predicate that also tests for spaces.
isSeparator :: Char -> Bool
isSeparator c = C.isSpace c || C.isSeparator c

-- This runs `takeWhile` with the predicate, conses the initial element to the
-- front, and creates a Token of the given type. Hence `Token` from `takeWhile`
-- -en. Or something.
tokenFromTaken :: Monad m   
               => FilePath                      -- ^ tokenSource
               -> Integer                       -- ^ tokenOffset
               -> TokenType                     -- ^ tokenType
               -> Char                          -- ^ Initial character.
               -> (Char -> Bool)                -- ^ Predicate for taking the rest of the token.
               -> E.Iteratee T.Text m Token
tokenFromTaken source offset tType initial predicate =
    -- The composed function here attaches the initial character, actualizes
    -- the token string from lazy to strict, and creates the `Token` from that.
    liftM (makeToken source offset tType . LT.toStrict . LT.cons initial)
          (ET.takeWhile predicate)

-- This takes the minimum data necessary to create a token, normalizes the
-- input text, and creates the `Token`.
makeToken :: FilePath -> Integer -> TokenType -> T.Text -> Token
makeToken source offset tType raw =
    Token normalized raw rawLength tType source offset
    where
        normalized = normalizeToken raw
        rawLength  = T.length raw

-- This takes a raw token `Text` and returns a normalized version. Currently,
-- this just lower-cases everything.
normalizeToken :: T.Text -> T.Text
normalizeToken = T.map C.toLower

