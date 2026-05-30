-- Usage:
--   uilayout <file.ull>           -- parse, layout, print resolved tree + ASCII art
--   uilayout <file.ull> --svg     -- also emit <file.svg> in the same directory
--   uilayout --demo               -- run the built-in demo layout

module Main where

import AST
import Parser
import Layout
import Renderer

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--demo"]         -> runDemo
    [path]             -> runFile path False
    [path, "--svg"]    -> runFile path True
    _                  -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: uilayout <file.ull> [--svg]"
  hPutStrLn stderr "       uilayout --demo"
  exitFailure

-- ---------------------------------------------------------------------------
-- Process a .ull file
-- ---------------------------------------------------------------------------
runFile :: FilePath -> Bool -> IO ()
runFile path emitSvg = do
  src <- readFile path
  case parseWindow src of
    Left e -> do
      hPutStrLn stderr (show e)
      exitFailure
    Right win -> do
      let resolved = resolveWindow win
      putStrLn $ "Window: " ++ winName win
              ++ "  " ++ show (winWidth win) ++ "x" ++ show (winHeight win)
      putStrLn ""
      printResolved 0 resolved
      putStrLn ""
      putStrLn $ toAscii win resolved
      if emitSvg
        then do
          let svgPath = reverse (drop 3 (reverse path)) ++ "svg"
          writeFile svgPath (toSvg win resolved)
          putStrLn $ "SVG written to: " ++ svgPath
        else return ()

printResolved :: Int -> Resolved -> IO ()
printResolved depth r = do
  let indent = replicate (depth * 2) ' '
  putStrLn $ indent
          ++ "Box x=" ++ show (rx r)
          ++ " y="    ++ show (ry r)
          ++ " w="    ++ show (rw r)
          ++ " h="    ++ show (rh r)
          ++ maybe "" (\c -> " color=" ++ c) (rColor r)
  mapM_ (printResolved (depth + 1)) (rChildren r)

-- ---------------------------------------------------------------------------
-- demo
-- ---------------------------------------------------------------------------
demoSrc :: String
demoSrc = unlines
  [ "window \"Main\" 800 x 600 {"
  , "  row {"
  , "    box { width: 20%, height: 100%, color: red  }"
  , "    box { width: 80%, height: 100%, color: blue }"
  , "  }"
  , "}"
  ]

runDemo :: IO ()
runDemo = do
  putStrLn "=== Demo Layout ==="
  putStrLn demoSrc
  case parseWindow demoSrc of
    Left e   -> hPutStrLn stderr (show e) >> exitFailure
    Right win -> do
      let resolved = resolveWindow win
      putStrLn "=== Resolved Tree ==="
      printResolved 0 resolved
      putStrLn ""
      putStrLn "=== ASCII Preview ==="
      putStrLn $ toAscii win resolved
      putStrLn "=== SVG ==="
      putStr $ toSvg win resolved