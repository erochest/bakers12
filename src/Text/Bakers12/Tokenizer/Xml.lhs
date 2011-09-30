
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
fastTokenizeFile :: Tokenizable a => String -> FilePath -> IO [a]
fastTokenizeFile idAttr inputFile = runX (fastTokenizeFile' idAttr inputFile)

fastTokenizeFile' :: Tokenizable a => String -> FilePath -> IOSArrow b a
fastTokenizeFile' idAttr inputFile =
    configSysVars [withValidate no] >>>
    readDocument [] inputFile >>>
    deep (hasAttr idAttr)

\end{code}

