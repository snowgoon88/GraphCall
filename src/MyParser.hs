module MyParser where

import qualified Text.Parsec as TP
import qualified Text.Parsec.Combinator as TPC
import Debug.Trace
-- import Text.Parsec (label)
-- import Text.Parsec.Token (GenTokenParser(reserved, dot))
-- import Control.Arrow (Kleisli(runKleisli))
-- import Data.Ratio (numerator)
-- import qualified Text.Parsec as TPD
-- import Text.Parsec (State(State))

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
  , level       :: Int        -- indent/hierarchy level
  , nbCall      :: Int
  }
  deriving (Show, Eq)
initParseInfo :: String -> ParseInfo
initParseInfo  nameOfFile = ParseInfo (initInfo nameOfFile) [] [] 0 0

-- *****************************************************************************
-- *********************************************************************** Debug
-- Manage indentation by increasing/decreasing StateInfo nLevel
incLevel :: TP.Parsec String ParseInfo ()
incLevel = do
  s <- TP.getState
  let nLevel = level s
  TP.setState (s { level = nLevel+2 })

decLevel :: TP.Parsec String ParseInfo ()
decLevel = do
  s <- TP.getState
  let nLevel = level s
  TP.setState (s { level = nLevel-2 })

-- turn ParseInfo nLevel into a String with nLevel '.''
indentState ::  TP.Parsec String ParseInfo String
indentState = do
  s <- TP.getState
  return (replicate (level s) '.')

-- to be called at begining of Parse rule
-- trace indended label and increase indentation
startTrace :: String -> TP.Parsec String ParseInfo ()
startTrace label = do
  prefix <- indentState
  let indentLabel = prefix ++ "__" ++ label
  TPC.parserTrace indentLabel
  incLevel
-- to be called at end of Parser rule
-- decrease indentation
endTrace = do
  decLevel

-- withDebug label rules wrap rules in startTrace label and endTrace
withDebug :: String -> TP.Parsec String ParseInfo a -> TP.Parsec String ParseInfo a
withDebug label rules = do
  startTrace label
  res <- rules
  endTrace
  return res
-- *****************************************************************************

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
-- ******************************************************************** Function
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
whitesTok = TP.skipMany sepTok >> TPC.parserTrace "W:"

whites1Tok :: TP.Parsec String ParseInfo ()
whites1Tok = TP.skipMany1 sepTok >> TPC.parserTrace "W1:"

-- WARN: TP.isSpace uses Data.Char.isSpace, True for " \t\n\r\v\f"
sepTok :: TP.Parsec String ParseInfo Char
sepTok = do
  TPC.parserTrace "S:"
  TP.oneOf "; \t\f\v" TP.<|> eolCountTok

eolCountTok :: TP.Parsec String ParseInfo Char
eolCountTok = do
  e <- TP.char '\n'
  state <- TP.getState
  let i' = incPos (currentPos state)
  TP.setState (state { currentPos = i' }) >> TPC.parserTrace "EOL:state+1"
  return e

-- TODO 2+3 is also a numerator
litteralTok:: TP.Parsec String ParseInfo String
litteralTok = do
  TP.try funcCallTokD
  TP.<|> TP.try collecTokD
  TP.<|> TP.try arrayTokD
  TP.<|> varnameTokD
  TP.<|> stringTokD
  TP.<|> numberTokD
litteralTokD = withDebug "litteral" litteralTok

varnameTok :: TP.Parsec String ParseInfo String
varnameTok = do
  l <- TP.letter TP.<|> TP.char '_'
  ls <- TP.many (TP.alphaNum TP.<|> TP.oneOf "_")
  TPC.parserTrace ("VAR l:"++show l++" ls:"++show ls)
  return (l:ls)
varnameTokD = withDebug "varname" varnameTok

numberTok :: TP.Parsec String ParseInfo String
numberTok = do
  n <- TP.many1 (TP.digit TP.<|> TP.oneOf "-e.")
  return ("NUM "++n)
numberTokD = withDebug "number" numberTok

stringTok :: TP.Parsec String ParseInfo String
stringTok = do
  s <- TP.between (TP.char '"') (TP.char '"') (TP.many (TP.noneOf ['"']))
  return ("STR "++s)
stringTokD = withDebug "string" stringTok

arrayTok :: TP.Parsec String ParseInfo String
arrayTok = do
  l <- varnameTokD --TODO ? TP.<|> funcCallTok
  i <-  TP.between (TP.char '[') (TP.char ']') inside
  return ("ARRAY "++l)
  where inside = do
          whitesTok
          ie <- litteralTokD
          whitesTok
          return ie
arrayTokD = withDebug "array" arrayTok

collecTok :: TP.Parsec String ParseInfo String
collecTok = do
  l <- varnameTokD
  TP.between (TP.char '<') (TP.char '>') inside
  return ("COLLEC "++l)
  where inside = do
          whitesTok
          ie <- varnameTokD
          whitesTok
          return ie
collecTokD = withDebug "collec" collecTok

funcCallTok :: TP.Parsec String ParseInfo String
funcCallTok = do
  startTrace "funccall"
  -- gard using State
  guardState <- TP.getState
  let n = nbCall guardState
  if n > 5
    then do TPC.parserTrace "GUARD"
            endTrace
            return "FUNCALL __STOP__"
    else do TP.setState (guardState { nbCall = n+1})
            f <- funcnameTokD
            state <- TP.getState
            p <- TP.between (TP.char '(') (TP.char ')') (TP.sepBy (do { whitesTok
                                                                      ; litteralTokD
                                                                      ; whitesTok })
                                                         (TP.char ','))
            let newFC = FuncInfo f "ukn" (length p) (currentPos state)
            state_end <- TP.getState
            TP.setState (state_end { calls = newFC: calls state_end })
            endTrace
            return ("FUNCALL "++f++"("++ show (length p) ++")")
funcCallTokD :: TP.Parsec String ParseInfo String
funcCallTokD = withDebug "funccall" funcCallTok

funcnameTok :: TP.Parsec String ParseInfo String
funcnameTok = do
  f <- TP.try collecTok
    TP.<|> TP.try arrayTok
    TP.<|> varnameTokD
  TPC.parserTrace ("FUNC "++ show f )
  return f
funcnameTokD = withDebug "funcname" funcnameTok

-- TODO parse funcCall avec, avant, des char any OU sep MAIS PAS {}
bodyTok :: TP.Parsec String ParseInfo [String]
bodyTok = do
  TP.many ( do { f <- TP.try funcCallTok
                      TP.<|> TP.many1 (TP.noneOf "{}; \t\f\v")
               ; whites1Tok
               ; return f })
bodyTokD = withDebug "body" bodyTok

wBodyTok :: TP.Parsec String ParseInfo [String]
wBodyTok = do
  whitesTok
  bodyTok
wBodyTokD = withDebug "wBody" wBodyTok

parseFunc :: TP.Parsec String ParseInfo a -> String -> Either TP.ParseError a
parseFunc rule = TP.runParser rule (initParseInfo "cin") "ukn"

parseFuncS :: TP.Parsec String ParseInfo a -> String -> Either TP.ParseError ParseInfo
parseFuncS rule = TP.runParser (rule >> TP.getState) (initParseInfo "cin") "ukn"

parseFuncF :: TP.Parsec String ParseInfo a -> FilePath -> IO (Either TP.ParseError a)
parseFuncF rule filename = do
  { input <- readFile filename
  ; let res = TP.runParser rule (initParseInfo filename) "file" input
  ; return res }

parseFuncFS :: TP.Parsec String ParseInfo a -> FilePath -> IO (Either TP.ParseError ParseInfo)
parseFuncFS rule filename = do
  { input <- readFile filename
  ; let res = TP.runParser (rule >> TP.getState) (initParseInfo filename) "file" input
  ; return res }
