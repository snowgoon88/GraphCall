module MyParser where

import qualified Text.Parsec as TP

-- text =
--   do results <- TP.many wordsTok
--      TP.eof
--      return results

-- TODO count whitespace ?

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
