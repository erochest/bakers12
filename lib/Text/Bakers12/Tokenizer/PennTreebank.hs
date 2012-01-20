
-- |
-- Module: Text.Bakers12.Tokenizer.PennTreebank
-- License : Apache 2.0
-- Maintainer: erochest@gmail.com
-- Portability: GHC
--
-- This filters the tokens output by Text.Bakers12.Tokenizer to make them
-- conform to the Penn Treebank tokenizer, more or less.
--
-- /More or less/ because this doesn't attempt to handle end-of-sentence
-- punctuation correctly. The original tokenizer required that each sentence be
-- on its own line. This doesn't use that.

module Text.Bakers12.Tokenizer.PennTreebank
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , pennFilter
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans (lift)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Bakers12.Tokenizer as B12
import           Text.Bakers12.Tokenizer.Types hiding (append, concat)
import qualified Text.Bakers12.Tokenizer.Types as Tkn

-- | This reads text from an instance of Data.Text.Text and returns a list of
-- Token instances.
tokenize :: FilePath -> T.Text -> Either SomeException [Token]
tokenize source input =
    E.runLists [[input]] process
    where process = B12.tokenizeStream source 0 E.=$ pennFilter E.=$ EL.consume

-- | This reads the input from a file and returns a list of Token instances.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (B12.tokenizeFileStream inputFile E.$= pennFilter E.$$ EL.consume)

-- | This creates an Enumerator that reads from a file and produces Tokens.
--
-- This assumes that the files are UTF8.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile =
    B12.tokenizeFileStream inputFile E.$= pennFilter

{- -- | This does some debugging tokenization. -}
{- tokenizeD :: FilePath -> T.Text -> IO (Either SomeException ()) -}
{- tokenizeD source input = -}
    {- E.run process -}
    {- where process = E.enumList 1 [input] E.$= B12.tokenizeStream source 0 -}
                    {- E.$= pennFilterIO -}
                    {- E.$$ E.printChunks True -}
                    {- -- E.$$ EL.consume -}

{- -- | This is an enumeratee that filters a token stream created by -}
{- -- Text.Bakers12.Tokenizer and makes it conform to the Penn Treebank tokenizer, -}
{- -- more or less. -}
{- pennFilterIO :: E.Enumeratee Token Token IO b -}
{- pennFilterIO cont@(E.Continue k) = do -}
    {- toDrop <- EL.takeWhile isSeparator -}
    {- lift . putStrLn . ("DROP " ++) $ show toDrop -}
    {- chunk <- EL.takeWhile (not . isSeparator) -}
    {- lift . putStrLn . ("TAKE " ++) $ show chunk -}
    {- if null chunk -}
    {- then return cont -}
    {- else do -}
        {- next  <- lift . E.runIteratee . k . E.Chunks $ penn chunk -}
        {- pennFilterIO next -}
    {- where isSeparator = (SeparatorToken ==) . tokenType -}
{- pennFilterIO step = return step -}

-- | This is an enumeratee that filters a token stream created by
-- Text.Bakers12.Tokenizer and makes it conform to the Penn Treebank tokenizer,
-- more or less.
pennFilter :: Monad m => E.Enumeratee Token Token m b
pennFilter cont@(E.Continue k) = do
    EL.dropWhile isSeparator
    chunk <- EL.takeWhile (not . isSeparator)
    case chunk of
        [] -> return cont
        _  -> do
            next  <- lift . E.runIteratee . k . E.Chunks $ penn chunk
            pennFilter next
    where isSeparator = (SeparatorToken ==) . tokenType
pennFilter step = return step

-- | This actually handles breaking everything apart and putting it back
-- together.
penn :: [Token] -> [Token]
penn (t:ts)
    | tokenText t == T.pack "\"" && L.all (not . isReadable) ts =
        t { tokenText = T.pack "''" } : penn ts
    | tokenText t == T.pack "\"" = t { tokenText = T.pack "``" } : penn ts
    -- brackets of various sorts.
    | T.singleton '(' == tokenText t = t { tokenText = T.pack "-LRB-" } : penn ts
    | T.singleton ')' == tokenText t = t { tokenText = T.pack "-RRB-" } : penn ts
    | T.singleton '[' == tokenText t = t { tokenText = T.pack "-LSB-" } : penn ts
    | T.singleton ']' == tokenText t = t { tokenText = T.pack "-RSB-" } : penn ts
    | T.singleton '{' == tokenText t = t { tokenText = T.pack "-LCB-" } : penn ts
    | T.singleton '}' == tokenText t = t { tokenText = T.pack "-RCB-" } : penn ts
    -- Run-ons.
    | tokenText t `elem` runOns = tsplit 3 t ++ penn ts
    where tsplit n tkn = let (t1, t2) = Tkn.splitAt n tkn
                         in  [t1, t2]
          runOns = map T.pack ["cannot", "gimme", "gonna", "gotta", "lemme", "wanna"]
penn (t1:t2:ts)
    | (tokenText t1 == dash) && (tokenText t2 == dash) =
        Tkn.append t1 t2 : penn ts
    | squote == tokenText t1 && tokenText t2 `elem` contractions =
        Tkn.append t1 t2 : penn ts
    | squote == tokenText t1 && tokenText t2 `elem` tcontractions =
        let (ttoken, trest) = Tkn.splitAt 1 t2
        in  Tkn.append t1 ttoken : trest : penn ts
    where dash          = T.singleton '-'
          squote        = T.pack "'"
          contractions  = map T.pack ["s", "ll", "m", "d", "re", "ve"]
          tcontractions = map T.pack ["tis", "twas"]
penn (t1:t2:t3:ts)
    | L.all (dot ==) . map tokenText $ take3 =
        Tkn.concat take3 : penn ts
    | T.last (tokenText t1) == n && tokenText t2 == squote && tokenText t3 == t =
        t1 { tokenText = T.init $ tokenText t1 } : nt : penn ts
    | tokenText t1 == d && tokenText t2 == squote && tokenText t3 == ye =
        Tkn.append t1 t2 : t3 : penn ts
    | tokenText t1 == more && tokenText t2 == squote && tokenText t3 == textn =
        t1 : Tkn.append t2 t3 : penn ts
    where dot    = T.singleton '.'
          squote = T.pack "'"
          d      = T.singleton 'd'
          n      = 'n'
          textn  = T.singleton n
          t      = T.singleton 't'
          ye     = T.pack "ye"
          more   = T.pack "more"
          nt     = t2 { tokenText   = T.pack "n't"
                      , tokenRaw    = T.pack "n't"
                      , tokenLength = 3
                      , tokenOffset = tokenOffset t2 - 1
                      }
          take3  = [t1, t2, t3]
penn (t:ts) = t : penn ts
penn []     = []

