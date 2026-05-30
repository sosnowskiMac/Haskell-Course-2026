-- Three categories:
--   1. Parser unit tests
--   2. Layout engine unit tests (hand-computed expected values)
--   3. End-to-end tests (parse → layout, check full resolved tree)
--   4. Property-based invariant tests

module Main where

import AST
import Parser
import Layout

import Data.List (intercalate)
import System.Exit (exitFailure, exitSuccess)

-- ---------------------------------------------------------------------------
-- Tiny test framework
-- ---------------------------------------------------------------------------

data TestResult = Pass String | Fail String String

runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "assertion failed"

runTestMsg :: String -> Bool -> String -> TestResult
runTestMsg name True  _   = Pass name
runTestMsg name False msg = Fail name msg

assert :: String -> Bool -> TestResult
assert = runTest

assertEqual :: (Show a, Eq a) => String -> a -> a -> TestResult
assertEqual name expected actual
  | expected == actual = Pass name
  | otherwise          = Fail name
      ("expected: " ++ show expected ++ "\n  got:      " ++ show actual)

printResults :: [TestResult] -> IO ()
printResults results = do
  let passes = [n | Pass n <- results]
  let fails  = [(n,m) | Fail n m <- results]
  mapM_ (\n -> putStrLn ("  PASS  " ++ n)) passes
  mapM_ (\(n,m) -> putStrLn ("  FAIL  " ++ n ++ "\n        " ++ m)) fails
  putStrLn $ show (length passes) ++ " passed, "
          ++ show (length fails) ++ " failed."
  if null fails then return () else exitFailure

-- ---------------------------------------------------------------------------
-- 1. Parser tests
-- ---------------------------------------------------------------------------

parserTests :: [TestResult]
parserTests =
  [ t_parseMinimalWindow
  , t_parseBoxWithProps
  , t_parseRowContainer
  , t_parseColContainer
  , t_parseNestedContainers
  , t_parseCommentLine
  , t_parseBlockComment
  , t_parsePxSize
  , t_parsePctSize
  , t_parseColor
  , t_parseBadInput
  , t_parseMissingBrace
  ]

t_parseMinimalWindow :: TestResult
t_parseMinimalWindow =
  case parseWindow "window \"W\" 100 x 200 { row {} }" of
    Left e  -> Fail "parseMinimalWindow" (show e)
    Right w -> assert "parseMinimalWindow"
                 (winName w == "W" && winWidth w == 100 && winHeight w == 200)

t_parseBoxWithProps :: TestResult
t_parseBoxWithProps =
  case parseWindow "window \"T\" 800 x 600 { box { width: 50%, height: 100px, color: red } }" of
    Left e  -> Fail "parseBoxWithProps" (show e)
    Right w ->
      let Box props _ = winRoot w
      in assertEqual "parseBoxWithProps"
           (Pct 0.5, Px 100, Just "red")
           (propWidth props, propHeight props, propColor props)

t_parseRowContainer :: TestResult
t_parseRowContainer =
  case parseWindow src of
    Left e  -> Fail "parseRowContainer" (show e)
    Right w ->
      let Box _ kids = winRoot w
      in assertEqual "parseRowContainer" 2 (length kids)
  where src = "window \"R\" 400 x 300 { row { box {} box {} } }"

t_parseColContainer :: TestResult
t_parseColContainer =
  case parseWindow "window \"C\" 400 x 300 { col { box {} box {} box {} } }" of
    Left e  -> Fail "parseColContainer" (show e)
    Right w ->
      let Box _ kids = winRoot w
      in assertEqual "parseColContainer" 3 (length kids)

t_parseNestedContainers :: TestResult
t_parseNestedContainers =
  case parseWindow "window \"N\" 400 x 300 { col { row { box {} } box {} } }" of
    Left e  -> Fail "parseNestedContainers" (show e)
    Right w ->
      let Box _ [child1, child2] = winRoot w
          Box _ grandkids        = child1
      in assertEqual "parseNestedContainers" (1, 0)
           (length grandkids, length (childrenOf child2))
  where childrenOf (Box _ cs) = cs

t_parseCommentLine :: TestResult
t_parseCommentLine =
  case parseWindow src of
    Left e  -> Fail "parseCommentLine" (show e)
    Right w -> assert "parseCommentLine" (winWidth w == 800)
  where src = unlines
          [ "-- This is a comment"
          , "window \"X\" 800 x 600 { row {} }"
          ]

t_parseBlockComment :: TestResult
t_parseBlockComment =
  case parseWindow src of
    Left e  -> Fail "parseBlockComment" (show e)
    Right w -> assert "parseBlockComment" (winWidth w == 800)
  where src = "/* ignored */ window \"X\" 800 x 600 { row {} }"

t_parsePxSize :: TestResult
t_parsePxSize =
  case parseWindow "window \"T\" 800 x 600 { box { width: 200px } }" of
    Left e  -> Fail "parsePxSize" (show e)
    Right w ->
      let Box p _ = winRoot w
      in assertEqual "parsePxSize" (Px 200) (propWidth p)

t_parsePctSize :: TestResult
t_parsePctSize =
  case parseWindow "window \"T\" 800 x 600 { box { width: 75% } }" of
    Left e  -> Fail "parsePctSize" (show e)
    Right w ->
      let Box p _ = winRoot w
      in assertEqual "parsePctSize" (Pct 0.75) (propWidth p)

t_parseColor :: TestResult
t_parseColor =
  case parseWindow "window \"T\" 800 x 600 { box { color: blue } }" of
    Left e  -> Fail "parseColor" (show e)
    Right w ->
      let Box p _ = winRoot w
      in assertEqual "parseColor" (Just "blue") (propColor p)

t_parseBadInput :: TestResult
t_parseBadInput =
  case parseWindow "this is not valid" of
    Left  _ -> Pass "parseBadInput"
    Right _ -> Fail "parseBadInput" "expected parse error, got success"

t_parseMissingBrace :: TestResult
t_parseMissingBrace =
  case parseWindow "window \"T\" 100 x 100 { row { box {}" of
    Left  _ -> Pass "parseMissingBrace"
    Right _ -> Fail "parseMissingBrace" "expected parse error, got success"

-- ---------------------------------------------------------------------------
-- 2. Layout engine unit tests
-- ---------------------------------------------------------------------------

layoutTests :: [TestResult]
layoutTests =
  [ t_singleBox100Pct
  , t_singleBoxPx
  , t_rowTwoEqual50Pct
  , t_rowTwoUnequalPct
  , t_colTwoEqual50Pct
  , t_rowOverflow
  , t_rowUnderflow
  , t_rowMixedPxPct
  , t_nestedRowInCol
  , t_singleBoxOrigin
  ]

t_singleBox100Pct :: TestResult
t_singleBox100Pct =
  let layout = Box (defaultProps { propWidth = Pct 1.0, propHeight = Pct 1.0 }) []
      r = resolveLayout 800 600 0 0 layout
  in assertEqual "singleBox100Pct" (800, 600, 0, 0) (rw r, rh r, rx r, ry r)

t_singleBoxPx :: TestResult
t_singleBoxPx =
  let layout = Box (defaultProps { propWidth = Px 200, propHeight = Px 300 }) []
      r = resolveLayout 800 600 0 0 layout
  in assertEqual "singleBoxPx" (200, 300) (rw r, rh r)

t_rowTwoEqual50Pct :: TestResult
t_rowTwoEqual50Pct =
  let child = Box (defaultProps { propWidth = Pct 0.5, propHeight = Pct 1.0 }) []
      parent = Box (defaultProps { propDir = Row }) [child, child]
      r = resolveLayout 800 600 0 0 parent
      [c1, c2] = rChildren r
  in assertEqual "rowTwoEqual50Pct"
       [(0, 0, 400, 600), (400, 0, 400, 600)]
       [(rx c1, ry c1, rw c1, rh c1), (rx c2, ry c2, rw c2, rh c2)]

t_rowTwoUnequalPct :: TestResult
t_rowTwoUnequalPct =
  let c1 = Box (defaultProps { propWidth = Pct 0.2 }) []
      c2 = Box (defaultProps { propWidth = Pct 0.8 }) []
      parent = Box (defaultProps { propDir = Row }) [c1, c2]
      r = resolveLayout 800 600 0 0 parent
      [r1, r2] = rChildren r
  in assertEqual "rowTwoUnequalPct"
       [(0, 160), (160, 640)]
       [(rx r1, rw r1), (rx r2, rw r2)]

t_colTwoEqual50Pct :: TestResult
t_colTwoEqual50Pct =
  let child = Box (defaultProps { propHeight = Pct 0.5 }) []
      parent = Box (defaultProps { propDir = Col }) [child, child]
      r = resolveLayout 800 600 0 0 parent
      [c1, c2] = rChildren r
  in assertEqual "colTwoEqual50Pct"
       [(0, 0, 800, 300), (0, 300, 800, 300)]
       [(rx c1, ry c1, rw c1, rh c1), (rx c2, ry c2, rw c2, rh c2)]

t_rowOverflow :: TestResult
t_rowOverflow =
  let child = Box (defaultProps { propWidth = Pct 0.8 }) []
      parent = Box (defaultProps { propDir = Row }) [child, child]
      r = resolveLayout 800 600 0 0 parent
      kids = rChildren r
  in assertEqual "rowOverflow" 800 (sum (map rw kids))

t_rowUnderflow :: TestResult
t_rowUnderflow =
  let child = Box (defaultProps { propWidth = Px 200 }) []
      parent = Box (defaultProps { propDir = Row }) [child, child]
      r = resolveLayout 800 600 0 0 parent
      [c1, c2] = rChildren r
  in assertEqual "rowUnderflow"
       (200, 600)
       (rw c1, rw c2)

t_rowMixedPxPct :: TestResult
t_rowMixedPxPct =
  let c1 = Box (defaultProps { propWidth = Px 100 }) []
      c2 = Box (defaultProps { propWidth = Pct 0.5 }) []
      parent = Box (defaultProps { propDir = Row }) [c1, c2]
      r = resolveLayout 800 600 0 0 parent
      [r1, r2] = rChildren r
  in
     assertEqual "rowMixedPxPct" (100, 700) (rw r1, rw r2)

t_nestedRowInCol :: TestResult
t_nestedRowInCol =
  let leaf = Box (defaultProps { propWidth = Pct 0.5 }) []
      row  = Box (defaultProps { propDir = Row }) [leaf, leaf]
      col  = Box (defaultProps { propDir = Col }) [row, row]
      r    = resolveLayout 400 200 0 0 col
      [row1, row2] = rChildren r
      [ll1, ll2]   = rChildren row1
  in assertEqual "nestedRowInCol"
       [(0,0,400,100), (0,0,200,100), (200,0,200,100)]
       [(rx row1, ry row1, rw row1, rh row1)
       ,(rx ll1, ry ll1, rw ll1, rh ll1)
       ,(rx ll2, ry ll2, rw ll2, rh ll2)]

t_singleBoxOrigin :: TestResult
t_singleBoxOrigin =
  let layout = Box defaultProps []
      r = resolveLayout 100 100 50 75 layout
  in assertEqual "singleBoxOrigin" (50, 75) (rx r, ry r)

-- ---------------------------------------------------------------------------
-- 3. End-to-end tests
-- ---------------------------------------------------------------------------

e2eTests :: [TestResult]
e2eTests =
  [ t_e2e_specExample
  , t_e2e_colLayout
  , t_e2e_deepNested
  ]


t_e2e_specExample :: TestResult
t_e2e_specExample =
  case parseWindow src of
    Left e  -> Fail "e2e_specExample" (show e)
    Right w ->
      let r = resolveWindow w
          [c1, c2] = rChildren r
      in assertEqual "e2e_specExample"
           [(0, 0, 160, 600, Just "red"), (160, 0, 640, 600, Just "blue")]
           [ (rx c1, ry c1, rw c1, rh c1, rColor c1)
           , (rx c2, ry c2, rw c2, rh c2, rColor c2) ]
  where
    src = unlines
      [ "window \"Main\" 800 x 600 {"
      , "  row {"
      , "    box { width: 20%, height: 100%, color: red  }"
      , "    box { width: 80%, height: 100%, color: blue }"
      , "  }"
      , "}"
      ]

t_e2e_colLayout :: TestResult
t_e2e_colLayout =
  case parseWindow src of
    Left e  -> Fail "e2e_colLayout" (show e)
    Right w ->
      let r = resolveWindow w
          kids = rChildren r
      in assertEqual "e2e_colLayout"
           [(0,0,200),(200,0,200),(400,0,200)]
           [(ry k, rx k, rh k) | k <- kids]
  where
    src = unlines
      [ "window \"C\" 400 x 600 {"
      , "  col {"
      , "    box { height: 200px }"
      , "    box { height: 200px }"
      , "    box { height: 200px }"
      , "  }"
      , "}"
      ]

-- Deeply nested layout: col > row > col > box
t_e2e_deepNested :: TestResult
t_e2e_deepNested =
  case parseWindow src of
    Left e  -> Fail "e2e_deepNested" (show e)
    Right w ->
      let r           = resolveWindow w
          [rowR]      = rChildren r
          [innerColR] = rChildren rowR
          [leafR]     = rChildren innerColR
      in assertEqual "e2e_deepNested"
           (0, 0, 400, 300)
           (rx leafR, ry leafR, rw leafR, rh leafR)
  where
    src = unlines
      [ "window \"D\" 400 x 300 {"
      , "  col {"
      , "    row {"
      , "      col {"
      , "        box { width: 100%, height: 100% }"
      , "      }"
      , "    }"
      , "  }"
      , "}"
      ]

-- ---------------------------------------------------------------------------
-- 4. Property-based tests
-- ---------------------------------------------------------------------------
type Seed = Int

nextSeed :: Seed -> Seed
nextSeed s = (s * 1664525 + 1013904223) `mod` (2^31)

randRange :: Seed -> Int -> Int -> (Int, Seed)
randRange s lo hi =
  let s' = nextSeed s
      v  = lo + (s' `mod` (hi - lo + 1))
  in  (v, s')

genLayout :: Seed -> Int -> (Layout, Seed)
genLayout s depth =
  let (choice, s1) = randRange s 0 (if depth >= 3 then 1 else 3)
  in  case choice of
        0 -> genLeaf s1
        1 -> genLeaf s1
        2 -> genContainer Row s1 depth
        _ -> genContainer Col s1 depth

genLeaf :: Seed -> (Layout, Seed)
genLeaf s =
  let (wChoice, s1) = randRange s  0 1
      (wVal,    s2) = randRange s1 0 100
      (hChoice, s3) = randRange s2 0 1
      (hVal,    s4) = randRange s3 0 100
      w = if wChoice == 0 then Px wVal else Pct (fromIntegral wVal / 100.0)
      h = if hChoice == 0 then Px hVal else Pct (fromIntegral hVal / 100.0)
      props = defaultProps { propWidth = w, propHeight = h }
  in  (Box props [], s4)

genContainer :: Direction -> Seed -> Int -> (Layout, Seed)
genContainer dir s depth =
  let (nKids, s1) = randRange s 1 4
      (kids, s2)  = genN nKids s1 depth
      props       = defaultProps { propDir = dir }
  in  (Box props kids, s2)

genN :: Int -> Seed -> Int -> ([Layout], Seed)
genN 0 s _     = ([], s)
genN n s depth =
  let (l,  s1) = genLayout s  (depth + 1)
      (ls, s2) = genN (n-1) s1 depth
  in  (l:ls, s2)

propChildInsideParent :: Resolved -> Bool
propChildInsideParent r =
  all (insideParent r) (rChildren r)
  && all propChildInsideParent (rChildren r)

insideParent :: Resolved -> Resolved -> Bool
insideParent parent child =
  rx child >= rx parent &&
  ry child >= ry parent &&
  rx child + rw child <= rx parent + rw parent &&
  ry child + rh child <= ry parent + rh parent

propChildrenSumFitsParent :: Resolved -> Bool
propChildrenSumFitsParent r =
  let kids     = rChildren r
      mainSize = if rDir r == Row then rw else rh
      total    = sum (map mainSize kids)
  in  total <= mainSize r
  &&  all propChildrenSumFitsParent kids

propDeterministic :: Layout -> Int -> Int -> Bool
propDeterministic layout w h =
  resolveLayout w h 0 0 layout == resolveLayout w h 0 0 layout

propertyTests :: [TestResult]
propertyTests = concatMap runOnePropertySuite seeds
  where
    seeds = [42, 137, 999, 3141, 27182, 100003, 99991, 1234567, 777, 12345]

runOnePropertySuite :: Seed -> [TestResult]
runOnePropertySuite seed =
  let (layout, s1) = genLayout seed 0
      (w, s2)      = randRange s1 50 1000
      (h, _)       = randRange s2 50 1000
      r            = resolveLayout w h 0 0 layout
      name suffix  = "property[seed=" ++ show seed ++ "] " ++ suffix
  in
    [ assert (name "childInsideParent")    (propChildInsideParent r)
    , assert (name "childrenSum")          (propChildrenSumFitsParent r)
    , assert (name "deterministic")        (propDeterministic layout w h)
    ]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Parser Tests ==="
  printResults parserTests
  putStrLn ""
  putStrLn "=== Layout Engine Tests ==="
  printResults layoutTests
  putStrLn ""
  putStrLn "=== End-to-End Tests ==="
  printResults e2eTests
  putStrLn ""
  putStrLn "=== Property-Based Tests ==="
  printResults propertyTests