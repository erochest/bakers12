
-- | This is the controller for the `tokenizer` mode. It runs the tokenizer and
-- prints out the output.

module Bakers12.Modes.Tokenizer
    ( tokenize
    ) where

import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.List as L
import           Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import           System.Bakers12.Enumerators (removeMissingFiles, expandDirectories)
import           System.IO (stdout)
import           Text.Bakers12.Tokenizer (Token(..), tokenizeE)
import           Text.Bakers12.Tokenizer.PennTreebank (pennFilter)

-- | This takes a list of possible file paths and tokenizes each one. It prints
-- the tokens out as CSV. Missing files are silently skipped and directories
-- are expanded into all the files in that directory and subdirectories. All of
-- this is handled with Enumerators, so it's memory consumption should be
-- decent.
tokenize :: [FilePath] -> IO ()
tokenize files =
    run_ (enumLists [files] $= removeMissingFiles $= expandDirectories $=
          tokenizeE $=
          pennFilter $=
          tokenToCsv $=
          ET.encode ET.utf8 $$
          EB.iterHandle stdout)

tokenToCsv :: Monad m => Enumeratee Token T.Text m b
tokenToCsv cont@(Continue k) = do
    maybeT <- EL.head
    case maybeT of
        Just token -> do
            next <- lift $ runIteratee $ k $ Chunks [showToken token]
            tokenToCsv next
        Nothing -> return cont
tokenToCsv step = return step

iterPutStrLn :: Iteratee String IO ()
iterPutStrLn = do
    maybeS <- EL.head
    case maybeS of
        Just output -> do
            lift $ putStrLn output
            iterPutStrLn
        Nothing -> return ()

showToken :: Token -> T.Text
showToken token@(Token tText tRaw tLen tType tSource tOffset) =
    TL.toStrict $ TB.toLazyText line
    where
        comma  = TB.singleton ','
        nl     = TB.singleton '\n'

        text   = TB.fromText . escape $ tText
        raw    = TB.fromText . escape $ tRaw
        len    = TBI.decimal tLen
        typ    = TB.fromString . show $ tType
        src    = TB.fromString tSource
        offs   = TBI.decimal tOffset

        fields = [ text, raw, len, typ, src ]

        push field builder = field `mappend` (comma `mappend` builder)
        line = foldr mappend (offs `mappend` nl) fields

tokenToList :: Monad m => Enumeratee Token [T.Text] m b
tokenToList cont@(Continue k) = do
    maybeT <- EL.head
    case maybeT of
        Just token -> do
            next <- lift $ runIteratee $ k $ Chunks [t2l token]
            tokenToList next
        Nothing -> return cont
tokenToList step = return step

showText :: Show a => a -> T.Text
showText = T.pack . show

t2l :: Token -> [T.Text]
t2l token = [ tokenText token
            , tokenRaw token
            , showText $ tokenLength token
            , showText $ tokenType token
            , T.pack   $ tokenSource token
            , showText $ tokenOffset token
            ]

listToCsvText :: Monad m => Enumeratee [T.Text] T.Text m b
listToCsvText cont@(Continue k) = do
    maybeL <- EL.head
    case maybeL of
        Just list -> do
            next <- lift $ runIteratee $ k $ Chunks [chunk]
            listToCsvText next
            where chunk = flip T.append nl
                        . T.intercalate comma
                        $ map escape list
                  nl = T.singleton '\n'
                  comma = T.singleton ','
        Nothing -> return cont
listToCsvText step = return step

escape :: T.Text -> T.Text
escape input = if T.any (not . C.isAlphaNum) input
               then T.concat [quote, escaped, quote]
               else input
    where
        quote   = T.singleton '"'
        escaped = T.replace quote (T.pack "\"\"") input

