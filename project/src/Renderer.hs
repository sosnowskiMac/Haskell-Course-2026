-- The SVG renderer produces a plain SVG file that can be viewed in any
-- browser.  Each box becomes a <rect> element, coloured by its `color`
-- property (defaulting to a light-grey fill with a dark border so that
-- the box hierarchy is always visible).
--
-- There is also a simple ASCII-art renderer for quick terminal inspection.

module Renderer
  ( toSvg
  , toAscii
  ) where

import AST
import Data.List (intercalate)

-- ---------------------------------------------------------------------------
-- SVG renderer
-- ---------------------------------------------------------------------------

toSvg :: Window -> Resolved -> String
toSvg win root =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<svg xmlns=\"http://www.w3.org/2000/svg\""
    , "     width=\""  ++ show (winWidth  win) ++ "\""
    , "     height=\"" ++ show (winHeight win) ++ "\""
    , "     viewBox=\"0 0 " ++ show (winWidth win) ++ " " ++ show (winHeight win) ++ "\">"
    , "  <title>" ++ escapeXml (winName win) ++ "</title>"
    , svgResolved root
    , "</svg>"
    ]

svgResolved :: Resolved -> String
svgResolved r =
  let fill   = maybe "#e8e8e8" id (rColor r)
      rectEl = "  <rect"
            ++ " x=\""      ++ show (rx r) ++ "\""
            ++ " y=\""      ++ show (ry r) ++ "\""
            ++ " width=\""  ++ show (rw r) ++ "\""
            ++ " height=\"" ++ show (rh r) ++ "\""
            ++ " fill=\""   ++ fill        ++ "\""
            ++ " stroke=\"#333\" stroke-width=\"1\""
            ++ "/>"
      kids = concatMap svgResolved (rChildren r)
  in rectEl ++ "\n" ++ kids

escapeXml :: String -> String
escapeXml = concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc '\'' = "&apos;"
    esc c    = [c]

-- ---------------------------------------------------------------------------
-- ASCII renderer
-- ---------------------------------------------------------------------------

asciiWidth  :: Int
asciiWidth  = 80

asciiHeight :: Int
asciiHeight = 40

toAscii :: Window -> Resolved -> String
toAscii win root =
  let scaleX x = (x * asciiWidth)  `div` max 1 (winWidth  win)
      scaleY y = (y * asciiHeight) `div` max 1 (winHeight win)
      blank = replicate asciiHeight (replicate asciiWidth '.')
      canvas = paint blank (scaleX, scaleY) root
  in  unlines canvas

paint :: [String]
      -> (Int -> Int, Int -> Int)
      -> Resolved
      -> [String]
paint canvas (sx, sy) r =
  let x0 = sx (rx r);  y0 = sy (ry r)
      x1 = sx (rx r + rw r);  y1 = sy (ry r + rh r)
      ch = colorChar (rColor r)
      canvas' = fillRect canvas x0 y0 x1 y1 ch
  in  foldl (\c kid -> paint c (sx, sy) kid) canvas' (rChildren r)

colorChar :: Maybe String -> Char
colorChar Nothing    = '+'
colorChar (Just col) = case col of
  "red"    -> 'R'
  "green"  -> 'G'
  "blue"   -> 'B'
  "yellow" -> 'Y'
  "white"  -> 'W'
  "black"  -> 'K'
  _        -> '#'

fillRect :: [String] -> Int -> Int -> Int -> Int -> Char -> [String]
fillRect rows x0 y0 x1 y1 ch =
  [ [ if row >= y0 && row < y1 && col >= x0 && col < x1
        then ch
        else c
    | (col, c) <- zip [0..] rowStr
    ]
  | (row, rowStr) <- zip [0..] rows
  ]
