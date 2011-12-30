
-- | This is the controller for the `tokenizer` mode. It runs the tokenizer and
-- prints out the output.

module Bakers12.Modes.Tokenizer
    ( tokenize
    ) where

import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import           System.Bakers12.Enumerators (removeMissingFiles, expandDirectories)
import           System.IO (stdout)
import           Text.Bakers12.Tokenizer (Token(..), tokenizeE)

-- | This takes a list of possible file paths and tokenizes each one. It prints
-- the tokens out as CSV. Missing files are silently skipped and directories
-- are expanded into all the files in that directory and subdirectories. All of
-- this is handled with Enumerators, so it's memory consumption should be
-- decent.
tokenize :: [FilePath] -> IO ()
tokenize files =
    run_ (enumLists [files] $= removeMissingFiles $= expandDirectories $=
          tokenizeE $=
          tokenToList $=
          listToCsvText $$
          ET.iterHandle stdout)

tokenToList :: Monad m => Enumeratee Token [T.Text] m b
tokenToList cont@(Continue k) = do
    maybeT <- EL.head
    case maybeT of
        Just token -> do
            next <- lift $ runIteratee $ k $ Chunks [list]
            tokenToList next
            where list = [ tokenText token
                         , tokenRaw token
                         , showText $ tokenLength token
                         , showText $ tokenType token
                         , T.pack   $ tokenSource token
                         , showText $ tokenOffset token
                         ]
                  showText :: Show a => a -> T.Text
                  showText = T.pack . show
        Nothing -> return cont
tokenToList step = return step

listToCsvText :: Monad m => Enumeratee [T.Text] T.Text m b
listToCsvText cont@(Continue k) = do
    maybeL <- EL.head
    case maybeL of
        Just list -> do
            next <- lift $ runIteratee $ k $ Chunks [chunk]
            listToCsvText next
            where chunk = flip T.snoc '\n'
                        . T.intercalate (T.singleton ',')
                        $ map escape list
        Nothing -> return cont
    where
        escape input =
            let i1 = T.replace (T.singleton '"') (T.pack "\"\"") input
            in  if T.any (not . C.isAlphaNum) i1
                then T.cons '"' $ T.snoc i1 '"'
                else input
listToCsvText step = return step

