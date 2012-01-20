
-- | Some utility Enumeratees used by the command-line program.

module System.Bakers12.Enumerators
    ( removeMissingFiles
    , expandDirectories
    , enumDirectory
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans (lift)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           System.Directory
import           System.FilePath


-- | This is an Enumeratee that removes missing files from a stream of file
-- names.
removeMissingFiles :: MonadIO m => E.Enumeratee FilePath FilePath m b
removeMissingFiles = EL.filterM (liftIO . exists)
    where exists :: FilePath -> IO Bool
          exists f = do
            isFile <- doesFileExist f
            isDir  <- doesDirectoryExist f
            return (isFile || isDir)

-- | This is an Enumeratee that expands directory names into all the files in
-- that directory tree.
expandDirectories :: MonadIO m => E.Enumeratee FilePath FilePath m b
expandDirectories cont@(E.Continue k) = do
    maybeFP <- EL.head
    case maybeFP of
        Just filePath -> do
            isFile <- liftIO $ doesFileExist filePath
            isDir  <- liftIO $ doesDirectoryExist filePath
            case (isFile, isDir) of
                (True, False)  -> do
                    next <- lift $ E.runIteratee $ k $ E.Chunks [filePath]
                    expandDirectories next
                (False, True)  -> do
                    next <- lift $ E.runIteratee $ k $ E.Chunks []
                    expandDirectories next E.>>== enumDirectory filePath
                _              -> do
                    next <- lift $ E.runIteratee $ k $ E.Chunks []
                    expandDirectories next
        Nothing -> return cont
expandDirectories step = return step

-- | This returns an Enumerator that walks over a directory tree.
--
-- This is taken from http://www.mew.org/~kazu/proj/enumerator/.
enumDirectory :: MonadIO m => FilePath -> E.Enumerator FilePath m b
enumDirectory dirname = list
    where
        list (E.Continue k) = do
            (files, dirs) <- liftIO getFilesDirs
            if null dirs
                then k (E.Chunks files)
                else k (E.Chunks files) E.>>== walk dirs
        list step = E.returnI step

        walk dirs = foldr1 (E.<==<) $ map enumDirectory dirs

        getFilesDirs = do
            cnts <- map (dirname </>) <$> getValidContents dirname
            (,) <$> filterM doesFileExist cnts
                <*> filterM isSearchableDir cnts

-- | This is also from http://www.mew.org/~kazu/proj/enumerator/, although it
-- may have originally come from Real World Haskell.
getValidContents :: FilePath -> IO [String]
getValidContents path =
    filter (`notElem` [".", ".."]) <$> getDirectoryContents path

-- | Another function from http://www.mew.org/~kazu/proj/enumerator/ or RWH.
isSearchableDir :: FilePath -> IO Bool
isSearchableDir dirname =
    (&&) <$> doesDirectoryExist dirname
         <*> (searchable <$> getPermissions dirname)

