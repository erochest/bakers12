
{-| System.Bakers12.Utils
 -
 - This is a collection of system utility functions that I've written for this
 - system.
 -}

module System.Bakers12.Utils
    ( normalizeFilePaths
    , getRecursiveContents
    , openBrowserOn
    , getResourceDir
    , isXml
    , trimBy
    ) where

import           Paths_bakers12 (getDataFileName)
import           Control.Monad (forM, liftM, liftM2, filterM, sequence, (=<<))
import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe (listToMaybe)
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getCurrentDirectory)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), dropFileName, takeExtension)
import           System.Info (os)
import           System.Process (readProcessWithExitCode)
import           Text.Printf (printf)


{-| normalizeFilePaths normalizes a list of file paths by removing ones that
 - don't exist and expanding directories into all of the files in their
 - subdirectories.
 -}

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

{-| This is from page 214 of *Real World Haskell* by Bryan O'Sullivan
 - (http://www.realworldhaskell.org/).
 -}

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

{-| This is borrowed with a slight change from
 - http://hackage.haskell.org/packages/archive/hledger/latest/doc/html/src/Hledger-Cli-Utils.html#openBrowserOn.
 -}

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

{-| This looks in several directories and returns the first that contains a
 - file named 'resource-dir-marker'. It assumes that the rest of the resources
 - lie under that directory.
 -}

getResourceDir :: IO (Maybe FilePath)
getResourceDir = do
    cwd <- getCurrentDirectory
    let dirPaths = [ return $ cwd </> "bakers12/resource-dir-marker"
                   , getDataFileName "bakers12/resource-dir-marker"
                   ]
    liftM (listToMaybe . map dropFileName) .
        filterM doesFileExist =<<
        sequence dirPaths

{-| These tests for whether a FilePath has an XML extension, for a pretty
 - limited range of XML.
 -}

xmlExts :: [String]
xmlExts = [ ".xml"
          ]

isXml :: FilePath -> Bool
isXml path = ext `elem` xmlExts
    where ext = map C.toLower . takeExtension $ path

{-| This trims any characters from the start of the list that match a
 - predicate.
 -}

trimBy :: (Char -> Bool) -> String -> String
trimBy pred = rtrim . ltrim
    where
        ltrim = L.dropWhile pred
        rtrim = L.reverse . ltrim . L.reverse

