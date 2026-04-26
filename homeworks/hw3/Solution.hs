module Homework03 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (permutations)
import Control.Monad (guard, mapM)
import Control.Monad.Writer (Writer, tell, writer, runWriter)

-- Ex 1

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos []     = Just pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos []     = Just [pos]
safePath maze pos (d:ds) = do
  next  <- move maze pos d
  rest  <- safePath maze next ds
  return (pos : rest)

-- Ex 2

type Key = Map Char Char

decodeChar :: Key -> Char -> Maybe Char
decodeChar key c = Map.lookup c key

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (decodeChar key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Ex 3

type Guest    = String
type Conflict = (Guest, Guest)

validSeating :: [Conflict] -> [Guest] -> Bool
validSeating conflicts gs = all noConflict adjacentPairs
  where
    adjacentPairs          = zip gs (tail gs ++ [head gs])
    noConflict (a, b)      = (a, b) `notElem` conflicts
                          && (b, a) `notElem` conflicts

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations guests
  guard (validSeating conflicts perm)
  return perm

-- Ex 4

data Result a = Failure String | Success a [String]
  deriving (Show)

instance Functor Result where
  fmap _ (Failure msg)      = Failure msg
  fmap f (Success x ws)     = Success (f x) ws

instance Applicative Result where
  pure x = Success x []

  Failure msg    <*> _              = Failure msg
  _              <*> Failure msg    = Failure msg
  Success f ws1  <*> Success x ws2  = Success (f x) (ws1 ++ ws2)

instance Monad Result where
  return = pure

  Failure msg   >>= _ = Failure msg
  Success x ws  >>= f =
    case f x of
      Failure msg      -> Failure msg
      Success y ws2    -> Success y (ws ++ ws2)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Age cannot be negative"
  | age > 150 = do { warn "Age above 150 is unusual"; return age }
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Ex 5

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show)

simplify :: Expr -> Writer [String] Expr

simplify (Add l r) = do

  l' <- simplify l
  r' <- simplify r
  case (l', r') of
    (Lit 0, e)           -> do { tell ["Add identity: 0 + e -> e"];   return e }
    (e, Lit 0)           -> do { tell ["Add identity: e + 0 -> e"];   return e }
    (Lit a, Lit b)       -> do { tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " = " ++ show (a+b)]; return (Lit (a+b)) }
    _                    -> return (Add l' r')

simplify (Mul l r) = do
  l' <- simplify l
  r' <- simplify r
  case (l', r') of
    (Lit 0, _)           -> do { tell ["Zero absorption: 0 * e -> 0"]; return (Lit 0) }
    (_, Lit 0)           -> do { tell ["Zero absorption: e * 0 -> 0"]; return (Lit 0) }
    (Lit 1, e)           -> do { tell ["Mul identity: 1 * e -> e"];    return e }
    (e, Lit 1)           -> do { tell ["Mul identity: e * 1 -> e"];    return e }
    (Lit a, Lit b)       -> do { tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " = " ++ show (a*b)]; return (Lit (a*b)) }
    _                    -> return (Mul l' r')

simplify (Neg e) = do
  e' <- simplify e
  case e' of
    Neg inner            -> do { tell ["Double negation: --e -> e"]; return inner }
    _                    -> return (Neg e')

simplify e = return e