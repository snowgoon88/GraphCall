module MyParser where

import qualified Text.Parsec as TP
import Debug.Trace

-- text =
--   do results <- TP.many wordsTok
--      TP.eof
--      return results

-- TODO count whitespace ?

-- *****************************************************************************
-- ************************************************************************ Info
-- To use as a State
data Info = Info
  { filename :: String
  , pos      :: Int
  }
  deriving (Eq)
instance Show Info where
  show info = (show (filename info)) ++ ":" ++ (show (pos info))

initInfo :: String -> Info
initInfo nameOfFile = Info nameOfFile 0

incPos :: Info -> Info
incPos info = Info { filename=(filename info), pos= (pos info) + 1 }
-- *****************************************************************************

wordsTok :: TP.Parsec String st [String]
wordsTok =
  do first <- wordTok
     next <- otherWordsTok
     return (first:next)
     
otherWordsTok :: TP.Parsec String st [String]      
otherWordsTok =
  (TP.char ' ' >> wordsTok) TP.<|> (TP.char '\n' >> wordsTok) TP.<|> (return [])

wordTok :: TP.Parsec String st String
wordTok =
  TP.many1 (TP.noneOf " \n")

parseTXT :: String -> Either TP.ParseError [String]
parseTXT input = TP.parse wordsTok ("ukn") input

-- *****************************************************************************
-- ****************************************************************** CountParse
--collWordsTok :: String -> Either TP.ParseError [(String,Info)]
collWordsTok = wordCountTok `TP.sepBy` whitesTok

wordCountTok :: TP.Parsec String Info (String,Info)
wordCountTok = do
  res <- TP.many1 (TP.noneOf " \n")
  info <- TP.getState
  trace ("WORD: " ++ res) $ return (res,info)

whitesTok :: TP.Parsec String Info ()
whitesTok = trace ("W:") $ TP.skipMany sepTok

-- WARN: TP.isSpace uses Data.Char.isSpace, True for " \t\n\r\v\f"
sepTok :: TP.Parsec String Info Char
sepTok = trace ("S:") $ (TP.oneOf " \t\f\v" TP.<|> eolCountTok)

eolCountTok :: TP.Parsec String Info Char
eolCountTok = do
  e <- TP.char '\n'
  info <- TP.getState
  let i' = incPos info
  trace ("EOL:state+1") $ TP.setState i'
  return e

-- *****************************************************************************
-- ********************************************************************* Function
funcTok :: TP.Parsec String Info [String]
funcTok = do
  t <- typeTok
  whitesTok
  f <- funcnameTok
  return [t,f]

typeTok :: TP.Parsec String st String
--typeTok = TP.letter >> TP.many (TP.alphaNum TP.<|> TP.oneOf "[<>]")
typeTok = do
  l <- TP.letter
  ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "[<>]_")
  trace ("TYPE l:"++(show l)++" ls:"++(show ls)) $ return (l:ls)

funcnameTok = do
  l <- TP.letter TP.<|> TP.char '_'
  ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_")
  trace ("FUNC l:"++(show l)++" ls:"++(show ls)) $ return (l:ls)
  
parseCountTXT :: String -> Either TP.ParseError [(String,Info)]
parseCountTXT input = TP.runParser (collWordsTok) (initInfo "ukn") ("ukn") input

  
