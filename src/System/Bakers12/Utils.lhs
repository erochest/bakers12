
System.Bakers12.Utils

This is a collection of system utility functions that I've written for this
system.

\begin{code}

module System.Bakers12.Utils
    ( normalizeFilePaths
    , getRecursiveContents
    ) where

import Control.Monad (forM, liftM2)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

\end{code}

normalizeFilePaths normalizes a list of file paths by removing ones that don't
exist and expanding directories into all of the files in their subdirectories.

\begin{code}

normalizeFilePaths :: [FilePath] -> IO [FilePath]
normalizeFilePaths [] = return []
normalizeFilePaths (f:fs) = do
    isFile <- doesFileExist f
    if isFile
        then normalizeFilePaths fs >>= (return . (f :))
        else liftM2 (++) (normDir f) (normalizeFilePaths fs)
    where
        normDir :: FilePath -> IO [FilePath]
        normDir dir = do
          isDir <- doesDirectoryExist dir
          if isDir
              then getRecursiveContents f
              else return []

\end{code}

This is from page 214 of *Real World Haskell* by Bryan O'Sullivan
(http://www.realworldhaskell.org/).

\begin{code}

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

\end{code}


