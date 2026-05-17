module Solution where

import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map


-- Ex 1

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
  s <- get
  case s of
    []     -> return ()
    (_:xs) -> put xs
execInstr DUP = do
  s <- get
  case s of
    []     -> return ()
    (x:xs) -> put (x : x : xs)
execInstr SWAP = do
  s <- get
  case s of
    (x:y:xs) -> put (y : x : xs)
    _         -> return ()
execInstr ADD = do
  s <- get
  case s of
    (x:y:xs) -> put (x + y : xs)
    _         -> return ()
execInstr MUL = do
  s <- get
  case s of
    (x:y:xs) -> put (x * y : xs)
    _         -> return ()
execInstr NEG = do
  s <- get
  case s of
    []     -> return ()
    (x:xs) -> put (negate x : xs)

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []

-- Ex 2

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n)       = return n
eval (Var x)       = do
  env <- get
  return (env Map.! x)
eval (Add e1 e2)   = (+) <$> eval e1 <*> eval e2
eval (Mul e1 e2)   = (*) <$> eval e1 <*> eval e2
eval (Neg e)       = negate <$> eval e
eval (Assign x e)  = do
  v <- eval e
  modify (Map.insert x v)
  return v
eval (Seq e1 e2)   = eval e1 >> eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- Ex 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just v  -> return v
    Nothing -> do
      v <- compute
      modify (Map.insert (i, j) v)
      return v
  where
    compute
      | i == 0    = return j
      | j == 0    = return i
      | xs !! (i-1) == ys !! (j-1) = editDistM xs ys (i-1) (j-1)
      | otherwise = do
          d1 <- editDistM xs ys (i-1) j
          d2 <- editDistM xs ys i     (j-1)
          d3 <- editDistM xs ys (i-1) (j-1)
          return (1 + minimum [d1, d2, d3])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- Ex 4 & 5

data Location
  = Start
  | Empty
  | DecisionPoint [String] [(String, Int)]
  | Obstacle String Int
  | Treasure String Int
  | Trap String Int
  | Goal
  deriving (Show)

data GameState = GameState
  { playerPos    :: Int
  , playerScore  :: Int
  , playerEnergy :: Int
  , visitedPos   :: [Int]
  , gameMessages :: [String]
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

board :: Map Int Location
board = Map.fromList
  [ (0,  Start)
  , (1,  Empty)
  , (2,  Treasure "Silver Coin" 10)
  , (3,  DecisionPoint
           ["Take the mountain path", "Take the forest path"]
           [("Take the mountain path", 4), ("Take the forest path", 7)])
  , (4,  Obstacle "Rockslide!" 1)
  , (5,  Treasure "Golden Goblet" 25)
  , (6,  Trap "Quicksand!" 15)
  , (7,  Obstacle "Dense fog" 2)
  , (8,  Treasure "Ancient Relic" 20)
  , (9,  Trap "Bandit ambush!" 20)
  , (10, Goal)
  ]

boardSize :: Int
boardSize = Map.size board - 1


getDiceRoll :: IO Int
getDiceRoll = do
  putStr "Roll the dice (enter 1-6): "
  line <- getLine
  let n = reads line :: [(Int, String)]
  case n of
    [(v, "")] | v >= 1 && v <= 6 -> return v
    _ -> do
      putStrLn "Invalid input. Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn "----------------------------------------"
  putStrLn $ "Position : " ++ show (playerPos gs) ++ " / " ++ show boardSize
  putStrLn $ "Score    : " ++ show (playerScore gs)
  putStrLn $ "Energy   : " ++ show (playerEnergy gs)
  case gameMessages gs of
    []   -> return ()
    msgs -> mapM_ (\m -> putStrLn $ "  >> " ++ m) msgs
  putStrLn "----------------------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "Choose your path:"
  mapM_ (\(i, o) -> putStrLn $ "  " ++ show i ++ ") " ++ o)
        (zip [1 :: Int ..] options)
  putStr "Your choice (enter number): "
  line <- getLine
  let n = reads line :: [(Int, String)]
  case n of
    [(v, "")] | v >= 1 && v <= length options -> return (options !! (v-1))
    _ -> do
      putStrLn "Invalid choice, try again."
      getPlayerChoice options


addMessage :: String -> AdventureGame ()
addMessage msg = modify (\gs -> gs { gameMessages = gameMessages gs ++ [msg] })

clearMessages :: AdventureGame ()
clearMessages = modify (\gs -> gs { gameMessages = [] })

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  gs <- get
  let oldPos = playerPos gs
      newPos  = min boardSize (oldPos + roll)
      moved   = newPos - oldPos
  put gs { playerPos    = newPos
          , playerEnergy = playerEnergy gs - 1
          , visitedPos   = visitedPos gs ++ [newPos] }
  addMessage ("Moved " ++ show moved ++ " spaces to position " ++ show newPos)
  return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- lift (getPlayerChoice options)
  addMessage ("You chose: " ++ choice)
  return choice

handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get
  let pos = playerPos gs
  case Map.lookup pos board of
    Nothing   -> return False
    Just Goal -> do
      addMessage "*** You reached the TREASURE! You WIN! ***"
      return True
    Just (Treasure desc pts) -> do
      modify (\s -> s { playerScore = playerScore s + pts })
      addMessage ("Treasure found: " ++ desc ++ " (+" ++ show pts ++ " points)")
      return False
    Just (Trap desc pts) -> do
      modify (\s -> s { playerScore = max 0 (playerScore s - pts) })
      addMessage ("Trap! " ++ desc ++ " (-" ++ show pts ++ " points)")
      return False
    Just (Obstacle desc back) -> do
      gs2 <- get
      let newPos = max 0 (playerPos gs2 - back)
      put gs2 { playerPos = newPos }
      addMessage ("Obstacle! " ++ desc ++ " (pushed back " ++ show back ++ " to pos " ++ show newPos ++ ")")
      return False
    Just (DecisionPoint options destinations) -> do
      choice <- makeDecision options
      let dest = case lookup choice destinations of
                   Just p  -> p
                   Nothing -> playerPos gs
      modify (\s -> s { playerPos = dest })
      addMessage ("Path leads to position " ++ show dest)
      return False
    Just _ -> return False

playTurn :: AdventureGame Bool
playTurn = do
  clearMessages
  gs <- get
  if playerEnergy gs <= 0
    then do
      addMessage "Out of energy! Game over."
      lift . displayGameState =<< get
      return True
    else do
      roll <- lift getDiceRoll
      addMessage ("Dice roll: " ++ show roll)
      _ <- movePlayer roll
      reached <- handleLocation
      lift . displayGameState =<< get
      return reached

playGame :: AdventureGame ()
playGame = do
  lift . displayGameState =<< get
  done <- playTurn
  if done
    then do
      gs <- get
      lift $ putStrLn $ "=== GAME OVER === Final Score: " ++ show (playerScore gs)
    else playGame

initialState :: GameState
initialState = GameState
  { playerPos    = 0
  , playerScore  = 0
  , playerEnergy = 15
  , visitedPos   = [0]
  , gameMessages = ["Welcome to Treasure Hunters! Reach position " ++ show boardSize ++ "."]
  }

runGame :: IO ()
runGame = evalStateT playGame initialState