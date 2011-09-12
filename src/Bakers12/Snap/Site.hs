{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Bakers12.Snap.Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Either
import           Data.Int (Int64)
import           Data.Maybe
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Snap.Types
import           Text.Templating.Heist
import qualified Text.XmlHtml as X

import           Bakers12.Snap.Application
import           Text.Bakers12.Tokenizer (Token(..))
import           Text.Bakers12.Tokenizer.String ()
import           Text.Bakers12.Utils (addTypeTokenRatio, fullTokenizeFile)


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
    heistLocal (bindSplice "tokens" $ tokenLoop tokenData)
        . heistLocal (bindSplice "ratioarray" $ ratioArray tokenData)
        $ render "tokenize"
    where
        -- TODO: handle errors
        processFiles :: [(PartInfo, Either PolicyViolationException FilePath)] -> Application [(Token String, Double)]
        processFiles parts =
            liftIO . liftM processTokens . mapM fullTokenizeFile . rights . map snd $ parts

        processTokens :: [[Token String]] -> [(Token String, Double)]
        processTokens = addTypeTokenRatio . concat

        partUploadPolicy :: PartInfo -> PartUploadPolicy
        partUploadPolicy _ = allowWithMaximumSize maxSize

        maxSize :: Int64
        maxSize = fromIntegral 5242880

tokenLoop :: [(Token String, Double)] -> Splice Application
tokenLoop tokens = do
    ts <- getTS
    node <- getParamNode
    let body = X.elementChildren node
    bds <- sequence . map (uncurry $ step body) $ tokens
    restoreTS ts
    return $ concat bds
    where

        step :: [X.Node] -> Token String -> Double -> Splice Application
        step body token ratio = do
            modifyTS $ bindSplices [ ("token", stringToSplice $ tokenText token)
                                   , ("raw", stringToSplice $ tokenRaw token)
                                   , ("source", stringToSplice $ tokenSource token)
                                   , ("offset", stringToSplice . show $ tokenOffset token)
                                   , ("length", stringToSplice . show $ tokenLength token)
                                   , ("ratio", stringToSplice . show $ ratio)
                                   ]
            runNodeList body

        stringToSplice :: Monad m => String -> Splice m
        stringToSplice string = return $ [convert string]
            where convert :: String -> X.Node
                  convert = X.TextNode . Text.pack

ratioArray :: [(Token String, Double)] -> Splice Application
ratioArray tokens = return $ [script ratios]
    where
        ratios :: String
        ratios =
            ("var ratioData = " ++) . toArray . map showPair . zip ns . map snd $ tokens

        script :: String -> X.Node
        script body =
            X.Element (Text.pack "script") [(Text.pack "type", Text.pack "text/javascript")]
                      [X.TextNode . Text.pack $ body]

        ns :: [Int]
        ns = L.iterate (1+) 1

        showPair :: (Int, Double) -> String
        showPair (n, ratio) = toArray [show n, show ratio]

        toArray :: [String] -> String
        toArray items = '[' : ar ++ "]"
            where ar = L.intercalate "," items


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/tokenize/",   tokenize)
             ]
       <|> serveDirectory "resources/static"
