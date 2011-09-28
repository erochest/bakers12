{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

This is the entry point for this web server application.  It supports
easily switching between interpreting source and running statically
compiled code.

In either mode, the generated program should be run from the root of
the project tree.  When it is run, it locates its templates, static
content, and source files in development mode, relative to the current
working directory.

When compiled with the development flag, only changes to the
libraries, your cabal file, or this file should require a recompile to
be picked up.  Everything else is interpreted at runtime.  There are a
few consequences of this.

First, this is much slower.  Running the interpreter takes a
significant chunk of time (a couple tenths of a second on the author's
machine, at this time), regardless of the simplicity of the loaded
code.  In order to recompile and re-load server state as infrequently
as possible, the source directories are watched for updates, as are
any extra directories specified below.

Second, the generated server binary is MUCH larger, since it links in
the GHC API (via the hint library).

Third, and the reason you would ever want to actually compile with
development mode, is that it enables a faster development cycle. You
can simply edit a file, save your changes, and hit reload to see your
changes reflected immediately.

When this is compiled without the development flag, all the actions
are statically compiled in.  This results in faster execution, a
smaller binary size, and having to recompile the server for any code
change.

-}

module Bakers12.Snap (serveSnap) where

#ifdef DEVELOPMENT
import           Control.Exception (SomeException, try)

import           Snap.Extension.Loader.Devel
import           Snap.Http.Server (quickHttpServe)
import           System.Bakers12.Utils (getResourceDir)
import           System.FilePath ((</>))
#else
import           Snap.Extension.Server
#endif

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Monoid (mempty)
import           System.Directory (createDirectoryIfMissing)
import           Bakers12.Snap.Application
import           Bakers12.Snap.Site
import           System.Bakers12.Utils (openBrowserOn, getResourceDir)


serveSnap :: Int -> IO ()
#ifdef DEVELOPMENT
serveSnap port = do
    -- All source directories will be watched for updates
    -- automatically.  If any extra directories should be watched for
    -- updates, include them here.
    putStrLn "DEV MODE"
    (snap, cleanup) <- $(let watchDirs = ["./bakers12/templates"]
                         in loadSnapTH 'devApplicationInitializer 'devSite watchDirs)
    forkIO launch
    try $ quickHttpServe snap :: IO (Either SomeException ())
    cleanup
    where launch :: IO ()
          launch = do
            threadDelay 1000
            openBrowserOn "http://localhost:8000"
            return ()
#else
serveSnap port = do
    createDirectoryIfMissing True "tmp"
    createDirectoryIfMissing True "log"
    maybeResourceDir <- getResourceDir
    case maybeResourceDir of
        Nothing -> putStrLn "No resource directory found. This won't end well."
        Just resourceDir -> do
            forkIO launch
            httpServe config (applicationInitializer resourceDir) (site resourceDir)
    where config :: ConfigExtend s
          config = setPort port mempty

          launch :: IO ()
          launch = do
            threadDelay 1000
            openBrowserOn . ("http://localhost:" ++) $ show port
            return ()
#endif
