-- Grammar (informal)
--
--   program   ::= window
--   window    ::= 'window' STRING INT 'x' INT '{' layout '}'
--   layout    ::= container | leaf
--   container ::= direction '{' layout* '}'
--   direction ::= 'row' | 'col'
--   leaf      ::= 'box' '{' prop* '}'
--   prop      ::= key ':' value ','?
--   key       ::= 'width' | 'height' | 'color' | 'dir'
--   value     ::= INT '%' | INT 'px'? | 'row' | 'col' | COLOR_NAME | HEX_COLOR
--
-- Comments: everything from '--' to end of line is ignored.
--           Block comments are /* … */

module Parser
  ( parseWindow
  , ParseError(..)
  ) where

import AST
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace, toLower)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------
data ParseError = ParseError
  { errLine :: Int
  , errCol  :: Int
  , errMsg  :: String
  } deriving (Eq)

instance Show ParseError where
  show e = "Parse error at line " ++ show (errLine e)
        ++ ", col "  ++ show (errCol e)
        ++ ": "      ++ errMsg e

-- ---------------------------------------------------------------------------
-- Parser state
-- ---------------------------------------------------------------------------
data St = St
  { stInput :: String
  , stLine  :: Int
  , stCol   :: Int
  } deriving (Show)

type Result a = Either ParseError a

initSt :: String -> St
initSt s = St s 1 1

-- ---------------------------------------------------------------------------
-- Low-level character operations
-- ---------------------------------------------------------------------------
advance :: St -> (Char, St)
advance st = case stInput st of
  []     -> error "advance past end"
  (c:cs) ->
    let (l', c') = if c == '\n' then (stLine st + 1, 1)
                                else (stLine st,     stCol st + 1)
    in  (c, St cs l' c')

peek :: St -> Maybe Char
peek st = case stInput st of
  []    -> Nothing
  (c:_) -> Just c

isEof :: St -> Bool
isEof = null . stInput

err :: St -> String -> Result a
err st msg = Left $ ParseError (stLine st) (stCol st) msg

-- ---------------------------------------------------------------------------
-- Whitespace & comment stripping
-- ---------------------------------------------------------------------------
skipWS :: St -> St
skipWS st
  | isEof st  = st
  | otherwise = case stInput st of
      ('-':'-':_)  -> skipWS (skipLine st)
      ('/':'*':_)  -> let (_, st2) = advance st
                          (_, st3) = advance st2
                      in  skipWS (skipBlockComment st3)
      (c:_)
        | isSpace c -> skipWS (snd (advance st))
        | otherwise -> st
      [] -> st

skipLine :: St -> St
skipLine st
  | isEof st  = st
  | otherwise = let (c, st') = advance st
                in  if c == '\n' then st' else skipLine st'

skipBlockComment :: St -> St
skipBlockComment st
  | isEof st = st
  | otherwise =
      case stInput st of
        ('*':'/':_) -> snd (advance (snd (advance st)))
        _           -> skipBlockComment (snd (advance st))

-- ---------------------------------------------------------------------------
-- Token helpers
-- ---------------------------------------------------------------------------
expectChar :: Char -> St -> Result St
expectChar c st =
  let st1 = skipWS st
  in case peek st1 of
       Nothing -> err st1 ("expected '" ++ [c] ++ "' but got end of input")
       Just c2
         | c == c2   -> Right (snd (advance st1))
         | otherwise -> err st1 ("expected '" ++ [c] ++ "' but got '" ++ [c2] ++ "'")

takeWhileFrom :: (Char -> Bool) -> St -> (String, St)
takeWhileFrom p st
  | isEof st  = ("", st)
  | otherwise = case peek st of
      Nothing -> ("", st)
      Just c
        | p c       -> let (cs, st') = takeWhileFrom p (snd (advance st))
                       in  (c:cs, st')
        | otherwise -> ("", st)

readIdent :: St -> Result (String, St)
readIdent st =
  let st1 = skipWS st
  in case peek st1 of
       Nothing -> err st1 "expected identifier but got end of input"
       Just c
         | isAlpha c || c == '_' ->
             let (rest, st2) = takeWhileFrom (\x -> isAlphaNum x || x == '_' || x == '-') (snd (advance st1))
             in  Right (c:rest, st2)
         | otherwise -> err st1 ("expected identifier but got '" ++ [c] ++ "'")

readInt :: St -> Result (Int, St)
readInt st =
  let st1 = skipWS st
  in case peek st1 of
       Nothing -> err st1 "expected integer but got end of input"
       Just c
         | isDigit c ->
             let (ds, st2) = takeWhileFrom isDigit st1
             in  Right (read ds, st2)
         | otherwise -> err st1 ("expected integer but got '" ++ [c] ++ "'")

readString :: St -> Result (String, St)
readString st =
  let st1 = skipWS st
  in case peek st1 of
       Just '"' ->
         let st2 = snd (advance st1)
             go acc s = case peek s of
               Nothing  -> err s "unterminated string literal"
               Just '"' -> Right (reverse acc, snd (advance s))
               Just c   -> go (c:acc) (snd (advance s))
         in  go [] st2
       _ -> err st1 "expected '\"'"

optComma :: St -> St
optComma st =
  let st1 = skipWS st
  in case peek st1 of
       Just ',' -> snd (advance st1)
       _        -> st1

optSemi :: St -> St
optSemi st =
  let st1 = skipWS st
  in case peek st1 of
       Just ';' -> snd (advance st1)
       _        -> st1

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------
parseWindow :: String -> Result Window
parseWindow input = do
  let st0 = initSt input
  (w, _) <- pWindow st0
  Right w

-- ---------------------------------------------------------------------------
-- window
-- ---------------------------------------------------------------------------
pWindow :: St -> Result (Window, St)
pWindow st0 = do
  st1     <- expectKeyword "window" st0
  (name, st2) <- readString st1
  (w, st3)    <- readInt st2
  st4     <- expectKeyword "x" st3
  (h, st5)    <- readInt st4
  st6     <- expectChar '{' st5
  (root, st7) <- pLayout st6
  st8     <- expectChar '}' st7
  Right (Window name w h root, st8)

expectKeyword :: String -> St -> Result St
expectKeyword kw st = do
  (tok, st') <- readIdent st
  if map toLower tok == map toLower kw
    then Right st'
    else err st ("expected keyword '" ++ kw ++ "' but got '" ++ tok ++ "'")

-- ---------------------------------------------------------------------------
-- layout ::= container | box
-- ---------------------------------------------------------------------------
pLayout :: St -> Result (Layout, St)
pLayout st0 = do
  let st1 = skipWS st0
  (tok, _) <- readIdent st1
  case map toLower tok of
    "row" -> pContainer Row st1
    "col" -> pContainer Col st1
    "box" -> pBox st1
    other -> err st1 ("expected 'row', 'col', or 'box' but got '" ++ other ++ "'")

pContainer :: Direction -> St -> Result (Layout, St)
pContainer dir st0 = do
  st1 <- expectKeyword (dirKeyword dir) st0
  st2 <- expectChar '{' st1
  (children, st3) <- pLayoutList st2
  st4 <- expectChar '}' st3
  let props = defaultProps { propDir = dir }
  Right (Box props children, st4)

dirKeyword :: Direction -> String
dirKeyword Row = "row"
dirKeyword Col = "col"

pLayoutList :: St -> Result ([Layout], St)
pLayoutList st0 = do
  let st1 = skipWS st0
  case peek st1 of
    Just '}' -> Right ([], st1)
    Nothing  -> err st1 "unexpected end of input inside container"
    _        -> do
      (l, st2) <- pLayout st1
      (ls, st3) <- pLayoutList st2
      Right (l:ls, st3)

pBox :: St -> Result (Layout, St)
pBox st0 = do
  st1 <- expectKeyword "box" st0
  st2 <- expectChar '{' st1
  (props, st3) <- pPropList defaultProps st2
  st4 <- expectChar '}' st3
  Right (Box props [], st4)

pPropList :: Props -> St -> Result (Props, St)
pPropList acc st0 = do
  let st1 = skipWS st0
  case peek st1 of
    Just '}' -> Right (acc, st1)
    Nothing  -> err st1 "unexpected end of input inside box"
    _        -> do
      (acc', st2) <- pProp acc st1
      pPropList acc' (optComma (optSemi st2))

pProp :: Props -> St -> Result (Props, St)
pProp acc st0 = do
  (key, st1) <- readIdent st0
  st2        <- expectChar ':' st1
  case map toLower key of
    "width"  -> do (sz, st3) <- pSize st2
                   Right (acc { propWidth  = sz }, st3)
    "height" -> do (sz, st3) <- pSize st2
                   Right (acc { propHeight = sz }, st3)
    "color"  -> do (c, st3) <- pColor st2
                   Right (acc { propColor  = Just c }, st3)
    "dir"    -> do (d, st3) <- pDir st2
                   Right (acc { propDir    = d }, st3)
    other    -> err st0 ("unknown property '" ++ other ++ "'")

pSize :: St -> Result (Size, St)
pSize st0 = do
  (n, st1) <- readInt st0
  let st2 = skipWS st1
  case peek st1 of
    Just '%'  -> Right (Pct (fromIntegral n / 100.0), snd (advance st1))
    Just 'p'  ->
      let st3 = snd (advance st1)
      in case peek st3 of
           Just 'x' -> Right (Px n, snd (advance st3))
           _        -> Right (Px n, st1)
    _         -> Right (Px n, st1)

pColor :: St -> Result (String, St)
pColor st0 = do
  let st1 = skipWS st0
  case peek st1 of
    Just '#' ->
      let (hex, st2) = takeWhileFrom (\c -> isAlphaNum c || c == '#') st1
      in  Right (hex, st2)
    _ -> do
      (tok, st2) <- readIdent st1
      Right (tok, st2)

pDir :: St -> Result (Direction, St)
pDir st0 = do
  (tok, st1) <- readIdent st0
  case map toLower tok of
    "row" -> Right (Row, st1)
    "col" -> Right (Col, st1)
    other -> err st0 ("expected 'row' or 'col' but got '" ++ other ++ "'")
