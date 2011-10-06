
Text.Bakers12.Tokenizer.Xml

This parses an XML file and tokenizes the text in it. If it finds an ID
attribute for the containing element, it tracks and adds that to the source
(sourcd#id).

These functions do no validation when reading the input.

\begin{code}
module Text.Bakers12.Tokenizer.Xml
    ( fullTokenizeFile
    , fastTokenizeFile
    ) where

import           Text.Bakers12.Tokenizer
import           Text.XML.HXT.Core
\end{code}

This performs fast text tokenization. It simply pulls out the text from the XML
file and tokenizes it.

\begin{code}
fastTokenizeFile :: Tokenizable a => FilePath -> IO [a]
fastTokenizeFile inputFile = runX (fastTokenizeFile' inputFile)

fastTokenizeFile' :: Tokenizable a => FilePath -> IOSArrow b a
fastTokenizeFile' inputFile =
    configSysVars [withValidate no] >>>
    readDocument [] inputFile >>>
    deep isText >>>
    getText >>>
    arrL (fastTokenize . fromString)

\end{code}

\begin{code}
fullTokenizeFile :: Tokenizable a => String -> String -> FilePath -> IO [Token a]
fullTokenizeFile idAttr source inputFile = return []
\end{code}

