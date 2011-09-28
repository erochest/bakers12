{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Bakers12.Snap.Site
  ( site
  , devSite
  ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Either
import           Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Text as T
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Snap.Types
import           Text.Templating.Heist
import qualified Text.XmlHtml as X

import           Bakers12.Snap.Application
import           System.Bakers12.Utils (getResourceDir)
import           System.FilePath ((</>))
import           Text.Bakers12.Tokenizer (Token(..))
import           Text.Bakers12.Tokenizer.Text (fullTokenizeFile)
import           Text.Bakers12.Stats (summarize)


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ render "index"


-- | Renders the tokenized files page.
tokenize :: Application ()
tokenize = do
    tokenData <- handleFileUploads "tmp" defaultUploadPolicy partUploadPolicy processFiles
    heistLocal (bindSplice "tokens" . tokenLoop $ fst tokenData)
        . heistLocal (bindSplice "ratioarray" . ratioArray $ fst tokenData)
        . heistLocal (bindSplice "freqarray" . freqArray $ snd tokenData)
        $ render "tokenize"
    where
        -- TODO: handle errors
        processFiles :: [(PartInfo, Either PolicyViolationException FilePath)] -> Application ([(Token T.Text, Double)], M.Map (Token T.Text) Int)
        processFiles parts =
            liftIO . liftM processTokens . mapM fullTokenizeFile . rights . map snd $ parts

        processTokens :: [[Token T.Text]]
                      -> ([(Token T.Text, Double)], M.Map (Token T.Text) Int)
        processTokens = summarize . concat

        partUploadPolicy :: PartInfo -> PartUploadPolicy
        partUploadPolicy _ = allowWithMaximumSize maxSize

        maxSize :: Int64
        maxSize = fromIntegral 5242880

tokenLoop :: [(Token T.Text, Double)] -> Splice Application
tokenLoop tokens = do
    ts <- getTS
    node <- getParamNode
    let body = X.elementChildren node
    bds <- sequence . map (uncurry $ step body) $ tokens
    restoreTS ts
    return $ concat bds
    where

        step :: [X.Node] -> Token T.Text -> Double -> Splice Application
        step body token ratio = do
            modifyTS $ bindSplices [ ("token", stringToSplice . T.unpack $ tokenText token)
                                   , ("raw", stringToSplice . T.unpack $ tokenRaw token)
                                   , ("source", stringToSplice $ tokenSource token)
                                   , ("offset", stringToSplice . show $ tokenOffset token)
                                   , ("length", stringToSplice . show $ tokenLength token)
                                   , ("ratio", stringToSplice . show $ ratio)
                                   ]
            runNodeList body

        stringToSplice :: Monad m => String -> Splice m
        stringToSplice string = return $ [convert string]
            where convert :: String -> X.Node
                  convert = X.TextNode . T.pack

ratioArray :: [(Token T.Text, Double)] -> Splice Application
ratioArray tokens = return $ [script ratios]
    where
        ratios :: String
        ratios =
            ("var ratioData = " ++) . toArray . map showPair . zip ns . map snd $ tokens

        ns :: [Int]
        ns = L.iterate (1+) 1

        showPair :: (Int, Double) -> String
        showPair (n, ratio) = toArray [show n, show ratio]

freqArray :: M.Map (Token T.Text) Int -> Splice Application
freqArray freqMap = return $ [script freqs]
    where
        freqs :: String
        freqs =
            ("var freqData = " ++)
                . toArray
                . map showPair
                . zip ns
                . L.sort
                . map snd
                $ M.toList freqMap

        ns :: [Int]
        ns = L.iterate (1+) 1

        showPair :: (Int, Int) -> String
        showPair (n, freq) = toArray [show n, show freq]

script :: String -> X.Node
script body =
    X.Element (T.pack "script") [(T.pack "type", T.pack "text/javascript")]
              [X.TextNode . T.pack $ body]

toArray :: [String] -> String
toArray items = '[' : ar ++ "]"
    where ar = L.intercalate "," items


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: FilePath -> Application ()
site resourceDir =
    route [ ("/",            index)
          , ("/tokenize/",   tokenize)
          ]
    <|> (serveDirectory $ resourceDir </> "static")

devSite :: Application ()
devSite = site "./bakers12"

