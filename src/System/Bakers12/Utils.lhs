
System.Bakers12.Utils

This is a collection of system utility functions that I've written for this
system.

\begin{code}

module System.Bakers12.Utils
    ( normalizeFilePaths
    , getRecursiveContents
    , openBrowserOn
    , getResourceDir
    ) where

import           Paths_bakers12 (getDataFileName)
import           Control.Monad (forM, liftM, liftM2, filterM, sequence, (=<<))
import qualified Data.List as L
import           Data.Maybe (listToMaybe)
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getCurrentDirectory)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), dropFileName)
import           System.Info (os)
import           System.Process (readProcessWithExitCode)
import           Text.Printf (printf)

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


This is borrowed with a slight change from
http://hackage.haskell.org/packages/archive/hledger/latest/doc/html/src/Hledger-Cli-Utils.html#openBrowserOn.

\begin{code}
-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ExitCode
openBrowserOn u = trybrowsers browsers u
    where
      trybrowsers (b:bs) u = do
        (e,_,_) <- readProcessWithExitCode b [u] ""
        case e of
          ExitSuccess -> return ExitSuccess
          ExitFailure _ -> trybrowsers bs u
      trybrowsers [] u = do
        putStrLn $ printf "Could not start a web browser (tried: %s)" $ L.intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u
        return $ ExitFailure 127
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["start"]
               | otherwise     = [ "sensible-browser"
                                 , "gnome-www-browser"
                                 , "google-chrome"
                                 , "firefox"
                                 ]
\end{code}

This looks in several directories and returns the first that contains a file
named 'resource-dir-marker'. It assumes that the rest of the resources lie
under that directory.

\begin{code}
getResourceDir :: IO (Maybe FilePath)
getResourceDir = do
    cwd <- getCurrentDirectory
    let dirPaths = [ return $ cwd </> "resources/resource-dir-marker"
                   , getDataFileName "resources/resource-dir-marker"
                   ]
    liftM (listToMaybe . map dropFileName) .
        filterM doesFileExist =<<
        sequence dirPaths
\end{code}

