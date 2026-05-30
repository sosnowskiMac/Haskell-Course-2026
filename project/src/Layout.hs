module Layout
  ( resolveWindow
  , resolveLayout
  ) where

import AST

-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

resolveWindow :: Window -> Resolved
resolveWindow win =
  resolveLayout (winWidth win) (winHeight win) 0 0 (winRoot win)

resolveLayout :: Int -> Int -> Int -> Int -> Layout -> Resolved
resolveLayout avW avH x0 y0 (Box props children) =
  let myW  = resolveSize avW (propWidth  props)
      myH  = resolveSize avH (propHeight props)
      rch  = placeChildren myW myH x0 y0 (propDir props) children
  in  Resolved x0 y0 myW myH (propColor props) rch

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

resolveSize :: Int -> Size -> Int
resolveSize parent (Px  n) = max 0 n
resolveSize parent (Pct f) = max 0 (round (f * fromIntegral parent))

-- ---------------------------------------------------------------------------
-- Child placement
-- ---------------------------------------------------------------------------

placeChildren :: Int -> Int -> Int -> Int -> Direction -> [Layout] -> [Resolved]
placeChildren _  _  _  _  _   []  = []
placeChildren pW pH ox oy dir chs =
  let
      mainParent  = if dir == Row then pW else pH
      crossParent = if dir == Row then pH else pW

      getSz (Box p _) = if dir == Row then propWidth p else propHeight p
      declared        = map (resolveSize mainParent . getSz) chs
      finals          = distributeMain mainParent declared

      place _ []     []     = []
      place cursor (m:ms) (child:rest) =
        let (cx, cy, cw, ch) = if dir == Row
              then (ox + cursor, oy, m, crossParent)
              else (ox, oy + cursor, crossParent, m)
            r = resolveLayout cw ch cx cy child
        in r : place (cursor + m) ms rest
      place _ _ _ = []

  in place 0 finals chs

-- ---------------------------------------------------------------------------
-- Distribute main-axis sizes
-- ---------------------------------------------------------------------------

distributeMain :: Int -> [Int] -> [Int]
distributeMain _      []  = []
distributeMain parent szs =
  let total = sum szs
  in  if total == 0
        then 
             let n = length szs
             in  replicate (n - 1) 0 ++ [parent]
      else if total <= parent
        then
             let surplus = parent - total
                 n = length szs
             in  take (n - 1) szs ++ [last szs + surplus]
        else 
             scaleProportional parent szs

scaleProportional :: Int -> [Int] -> [Int]
scaleProportional target szs =
  let total = sum szs
      scaled = map (\s -> (s * target) `div` total) szs
      err    = target - sum scaled
      n      = length scaled
  in  take (n - 1) scaled ++ [last scaled + err]
