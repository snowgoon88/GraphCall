module MyParser where

import qualified Text.Parsec as TP
import qualified Text.Parsec.Combinator as TPC
import Debug.Trace

-- text =
--   do results <- TP.many wordsTok
--      TP.eof
--      return results

-- TODO count whitespace ?

-- *****************************************************************************
-- ************************************************************************ Info
data ParseInfo = ParseInfo
  { currentPos  :: Info
  , defs        :: [FuncInfo]
  , calls       :: [FuncInfo]
  }
  deriving (Show, Eq)
initParseInfo :: String -> ParseInfo
initParseInfo  nameOfFile = ParseInfo (initInfo nameOfFile) [] []

-- About Func Def
data FuncInfo = FuncInfo
  { name     :: String
  , context  :: String
  , nbArg    :: Int
  , location :: Info
  }
  deriving (Eq)
instance Show FuncInfo where
  show f = show (location f) ++ " DEF " ++ show (context f) ++ "::" ++ show (name f) ++"( "++show (nbArg f)++" )"

-- To use as a State
data Info = Info
  { filename :: String
  , pos      :: Int
  }
  deriving (Eq)
instance Show Info where
  show info = show (filename info) ++ ":" ++ show (pos info)

initInfo :: String -> Info
initInfo nameOfFile = Info nameOfFile 0

incPos :: Info -> Info
incPos info = Info { filename=filename info, pos= pos info + 1 }


-- *****************************************************************************
-- wordsTok :: TP.Parsec String st [String]
-- wordsTok =
--   do first <- wordTok
--      next <- otherWordsTok
--      return (first:next)

-- otherWordsTok :: TP.Parsec String st [String]      
-- otherWordsTok =
--   (TP.char ' ' >> wordsTok) TP.<|> (TP.char '\n' >> wordsTok) TP.<|> (return [])

-- wordTok :: TP.Parsec String st String
-- wordTok =
--   TP.many1 (TP.noneOf " \n")

-- parseTXT :: String -> Either TP.ParseError [String]
-- parseTXT input = TP.parse wordsTok ("ukn") input

-- *****************************************************************************
-- ****************************************************************** CountParse
-- --collWordsTok :: String -> Either TP.ParseError [(String,Info)]
-- collWordsTok = wordCountTok `TP.sepBy` whitesTok

-- wordCountTok :: TP.Parsec String ParseInfo (String,Info)
-- wordCountTok = do
--   res <- TP.many1 (TP.noneOf " \n")
--   info <- TP.getState
--   trace ("WORD: " ++ res) $ return (res,info)


-- *****************************************************************************
-- ********************************************************************* Function
-- funcDefTok :: TP.Parsec String Info [[String]]
-- funcDefTok = do
--   t <- typeTok
--   whitesTok
--   f <- funcnameTok
--   whitesTok
--   a <- TP.between (TP.char '(') (TP.char ')') argsTok
--   balancedBrakTok
--   return (["FUNC",t,f]:a)

-- argsTok :: TP.Parsec String Info [[String]]
-- argsTok = do
--    a <- argTok `TPC.sepBy` (TP.char ',')
--    return a

-- argTok :: TP.Parsec String Info [String]
-- argTok = do
--   whitesTok
--   t <- typeTok
--   whitesTok
--   f <- TPC.option "noname" funcnameTok
--   whitesTok
--   return ["ARG", t, f]

-- typeTok :: TP.Parsec String st String
-- --typeTok = TP.letter >> TP.many (TP.alphaNum TP.<|> TP.oneOf "[<>]")
-- typeTok = do
--   l <- TP.letter
--   ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "[<>]_")
--   trace ("TYPE l:"++(show l)++" ls:"++(show ls)) $ return (l:ls)

-- funcnameTok :: TP.Parsec String Info String
-- funcnameTok = do
--   l <- TP.letter TP.<|> TP.char '_'
--   ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_")
--   trace ("FUNC l:"++(show l)++" ls:"++(show ls)) $ return (l:ls)

-- funcCallTok :: TP.Parsec String Info [[String]]
-- funcCallTok = do
--   v <- TPC.option "novar" (TP.try (do { vn <-varnameTok
--                                       ; TP.char '.'
--                                       ; return vn }))
--   f <- funcnameTok
--   whitesTok
--   p <- TP.between (TP.char '(') (TP.char ')') paramsTok
--   return (["CALL",v,f]:p)

-- paramsTok :: TP.Parsec String Info [[String]]
-- paramsTok = do
--    p <- paramTok `TPC.sepBy` (TP.char ',')
--    return p

-- paramTok :: TP.Parsec String Info [String]
-- paramTok = do
--   whitesTok
--   p <- TP.many( TP.alphaNum TP.<|> TP.oneOf ".[]\"_")
--   whitesTok
--   return ["PARAM", p]

-- varnameTok :: TP.Parsec String Info String
-- varnameTok = do
--   l <- TP.letter TP.<|> TP.char '_'
--   ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_[]\"")
--   trace ("VAR l:"++(show l)++" ls:"++(show ls)) $ return (l:ls)

-- parseCountTXT :: String -> Either TP.ParseError [(String,Info)]
-- parseCountTXT input = TP.runParser (collWordsTok) (initInfo "ukn") ("ukn") input


balancedBrakTok :: TP.Parsec String st ()
balancedBrakTok = do
  go
  return ()
  where go = do { TP.skipMany (TP.noneOf "{}")
                ; TP.many (do { TP.between (TP.char '{') (TP.char '}') go
                             ; TP.skipMany (TP.noneOf "{}")})
                }

-- possibleCallTok = do
--   TP.skipMany (TP.noneOf "{}")
--   TP.many (do { TP.skipMany (TP.noneOf "{}")
--               ; fc <- funcCallTo


-- ******************************************************************************
-- ************************************************************ funcCall Elements
whitesTok :: TP.Parsec String ParseInfo ()
whitesTok = trace "W:" $ TP.skipMany sepTok

-- WARN: TP.isSpace uses Data.Char.isSpace, True for " \t\n\r\v\f"
sepTok :: TP.Parsec String ParseInfo Char
sepTok = trace "S:" $ (TP.oneOf " \t\f\v" TP.<|> eolCountTok)

eolCountTok :: TP.Parsec String ParseInfo Char
eolCountTok = do
  e <- TP.char '\n'
  state <- TP.getState
  let i' = incPos (currentPos state)
  trace "EOL:state+1" $ TP.setState (state { currentPos = i' })
  return e

litteralTok:: TP.Parsec String ParseInfo String
litteralTok =
  TP.try funcCallTok
  TP.<|> stringTok
  TP.<|> TP.try arrayTok
  TP.<|> varnameTok
  TP.<|> numberTok


varnameTok :: TP.Parsec String ParseInfo String
varnameTok = do
  l <- TP.letter TP.<|> TP.char '_'
  ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_")
  trace ("VAR l:"++show l++" ls:"++show ls) $ return (l:ls)

numberTok :: TP.Parsec String ParseInfo String
numberTok = do
  n <- TP.many1 (TP.digit TP.<|> TP.oneOf "-e.")
  return ("NUM "++n)

stringTok :: TP.Parsec String ParseInfo String
stringTok = do
  s <- TP.between (TP.char '"') (TP.char '"') (TP.many (TP.noneOf ['"']))
  return ("STR "++s)

arrayTok :: TP.Parsec String ParseInfo String
arrayTok = do
  l <- varnameTok TP.<|> funcCallTok
  i <-  TP.between (TP.char '[') (TP.char ']') inside
  return ("ARRAY "++l)
  where inside = do
          whitesTok
          ie <- litteralTok
          whitesTok
          return ie

funcCallTok :: TP.Parsec String ParseInfo String
funcCallTok = do
  f <- funcnameTok
  whitesTok
  p <- TP.between (TP.char '(') (TP.char ')') (TP.sepBy (do { whitesTok
                                                            ; litteralTok
                                                            ; whitesTok })
                                                (TP.char ','))
  state <- TP.getState
  let newFC = FuncInfo f "ukn" (length p) (currentPos state)
  TP.setState (state { calls = newFC: calls state })
  return ("FUNCALL "++f++"("++ show (length p) ++")")

funcnameTok :: TP.Parsec String ParseInfo String
funcnameTok = do
  l <- TP.letter TP.<|> TP.char '_'
  ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_")
  trace ("FUNC l:"++ show l ++" ls:"++ show ls) $ return (l:ls)

parseFunc :: TP.Parsec String ParseInfo a -> String -> Either TP.ParseError a
parseFunc rule = TP.runParser rule (initParseInfo "cin") "ukn"

parseFuncS :: TP.Parsec String ParseInfo a -> String -> Either TP.ParseError ParseInfo
parseFuncS rule = TP.runParser (rule >> TP.getState) (initParseInfo "cin") "ukn"
