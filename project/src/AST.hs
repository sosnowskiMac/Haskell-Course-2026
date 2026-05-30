module AST where

-- ---------------------------------------------------------------------------
-- Size
-- ---------------------------------------------------------------------------
data Size
  = Px  Int     -- absolute pixels
  | Pct Double  -- fraction of parent
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Direction
-- ---------------------------------------------------------------------------

data Direction = Row | Col
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Props
-- ---------------------------------------------------------------------------
data Props = Props
  { propWidth  :: Size
  , propHeight :: Size
  , propDir    :: Direction
  , propColor  :: Maybe String
  } deriving (Eq, Show)

defaultProps :: Props
defaultProps = Props
  { propWidth  = Pct 1.0
  , propHeight = Pct 1.0
  , propDir    = Row
  , propColor  = Nothing
  }

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------
data Layout = Box Props [Layout]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Window declaration
-- ---------------------------------------------------------------------------
data Window = Window
  { winName   :: String
  , winWidth  :: Int
  , winHeight :: Int
  , winRoot   :: Layout
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Resolved
-- ---------------------------------------------------------------------------

data Resolved = Resolved
  { rx       :: Int
  , ry       :: Int
  , rw       :: Int
  , rh       :: Int
  , rColor   :: Maybe String
  , rChildren :: [Resolved]
  } deriving (Eq, Show)
