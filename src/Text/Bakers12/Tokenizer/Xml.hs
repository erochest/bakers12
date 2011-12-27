
Text.Bakers12.Tokenizer.Xml

This parses an XML file and tokenizes the text in it. If it finds an ID
attribute for the containing element, it tracks and adds that to the source
(sourcd#id).

These functions do no validation when reading the input.

\begin{code}
module Text.Bakers12.Tokenizer.Xml
    ( fullTokenize
    , fastTokenize
    , fullTokenizeFile
    , fastTokenizeFile
    ) where

import qualified Data.List as L
import           Data.Maybe
import qualified Text.Bakers12.Tokenizer as T
import           Text.XML.HXT.Core
\end{code}

This performs fast text tokenization. It simply pulls out the text from the XML
file and tokenizes it.

\begin{code}
fastTokenize :: T.Tokenizable a => a -> [a]
fastTokenize = runLA ft . T.toString
    where
        ft :: T.Tokenizable a => LA String a
        ft = xread >>> fastTokenize'

fastTokenizeFile :: T.Tokenizable a => FilePath -> IO [a]
fastTokenizeFile inputFile = runX (ftf inputFile)
    where
        ftf :: T.Tokenizable a => FilePath -> IOSArrow b a
        ftf inputFile =
            configSysVars [withValidate no] >>>
            readDocument [] inputFile >>>
            fromLA fastTokenize'

fastTokenize' :: T.Tokenizable b => LA XmlTree b
fastTokenize' =
    deep isText >>>
    getText >>>
    arrL (T.fastTokenize . T.fromString)
\end{code}

This performs full tokenization. Offsets are computed relative to the current
text data. The source is amended with the value of @id, if one of data's
parents has such. I'll break this down and explain it in more detail as we go.

First, we define a datatype, IdState. This tracks the stack of IDs that we're
currently in, and it contains a text buffer for holding the text data seen,
with the IDs (if any) that apply.

Following this data are some functions for working with IdStates outside of
arrows.

\begin{code}
data IdState = IdState { idStack    :: [String]
                       , textBuffer :: [(Maybe String, String)]
                       }
    deriving (Show)

-- | Initial state.
initIdState :: IdState
initIdState = IdState [] []

-- | Push a string ID onto the ID stack.
pushIdStack :: String -> IdState -> IdState
pushIdStack id state@(IdState ids _) = state { idStack=(id:ids) }

-- | Pop the top value from the ID stack, if there is one. Otherwise, leave the
-- state unchanged.
popIdStack :: IdState -> IdState
popIdStack state@(IdState []      _) = state
popIdStack state@(IdState (_:ids) _) = state { idStack=ids }

-- | This pushes some output onto the top of the text buffer. This also
-- includes the top of the idStack, if it's available.
pushTextBuffer :: String -> IdState -> IdState
pushTextBuffer text state@(IdState ids tb) =
    state { textBuffer=((ids', text):tb) }
    where ids' = listToMaybe ids
\end{code}

These functions make it eaiser to work with IdStates in the context of an
IOStateArrow.

\begin{code}
-- | Push a possible ID onto the ID stack of the current state.
pushId :: IOStateArrow IdState (Maybe String) (Maybe String)
pushId = changeUserState (pushIdStack . fromJust) `whenP` isJust

-- | When the ID has a value, pop from the ID stack.
popId :: IOStateArrow IdState (Maybe String) (Maybe String)
popId = changeUserState (\_ -> popIdStack) `whenP` isJust

-- | Return the top of the ID stack.
peekId :: IOStateArrow IdState a (Maybe String)
peekId = getUserState >>> arr (listToMaybe . idStack)

-- | This pushes text onto the textBuffer of the current state.
pushText :: IOStateArrow IdState String String
pushText = changeUserState pushTextBuffer
\end{code}

Now we can write the tokenizer.

\begin{code}
fullTokenize :: T.Tokenizable a => String -> String -> a -> IO [T.Token a]
fullTokenize source idAttr input = do
    text <- runX ft
    return . L.concatMap (uncurry (tokenize' source)) $ text
    where
        input' = T.toString input

        ft :: IOSArrow a (Maybe String, String)
        ft = withOtherUserState initIdState ft'

        ft' :: IOStateArrow IdState a (Maybe String, String)
        ft' =
            configSysVars [withValidate no] >>>
            readString [] input' >>>
            fullTokenize' idAttr

tokenize' :: T.Tokenizable a => String -> Maybe String -> String -> [T.Token a]
tokenize' source Nothing        = T.fullTokenize source . T.fromString
tokenize' source (Just idValue) =
    T.fullTokenize (source ++ ('#':idValue)) . T.fromString

fullTokenizeFile :: T.Tokenizable a => String -> String -> FilePath -> IO [T.Token a]
fullTokenizeFile idAttr source inputFile = do
    text <- runX (fullTokenizeFile' idAttr inputFile)
    return . L.concatMap (uncurry (tokenize' source)) $ text

-- | This is the arrow function. It just parses the file and passes everything
-- off to getElementText.
fullTokenizeFile' :: String -> FilePath -> IOSArrow a (Maybe String, String)
fullTokenizeFile' idAttr inputFile = withOtherUserState initIdState ftf'
    where
        ftf' :: IOStateArrow IdState a (Maybe String, String)
        ftf' =
            configSysVars [withValidate no] >>>
            readDocument [] inputFile >>>
            fullTokenize' idAttr

fullTokenize' :: String -> IOStateArrow IdState XmlTree (Maybe String, String)
fullTokenize' idAttr =
    processChildren (getElementText idAttr) >>>
    getUserState >>>
    arrL (L.reverse . textBuffer)

getElementText :: String -> IOStateArrow IdState XmlTree XmlTree
getElementText idAttr =
    ((getElemId idAttr >>> pushId) &&& returnA) >>>
    (getOutputText `when` (arr snd >>> isText)) >>>
    (returnA *** (processChildren (getElementText idAttr))) >>>
    (popId *** returnA) >>>
    arr snd

getElemId :: String -> IOStateArrow IdState XmlTree (Maybe String)
getElemId idAttr =
    getAttrValue idAttr >>>
    ifP L.null (arr alwaysNothing) (arr Just)
    where alwaysNothing _ = Nothing

getOutputText :: IOStateArrow IdState (Maybe String, XmlTree) (Maybe String, XmlTree)
getOutputText =
    returnA *** ((getOutputText' &&& returnA) >>> arr snd)
    where
        getOutputText' :: IOStateArrow IdState XmlTree String
        getOutputText' = getText >>> pushText

\end{code}

