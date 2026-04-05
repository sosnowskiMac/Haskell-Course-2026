module Homework02 where

import Data.Monoid (Sum(..))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

instance Show a => Show (Sequence a) where
    show Empty = "Empty"
    show (Single x) = "Single " ++ show x
    show (Append l r) = "Append (" ++ show l ++ ") (" ++ show r ++ ")"

-- Ex 1

instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x)  = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- Ex 2

instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
-- seqToList = foldr (:) []
seqToList = foldMap (\x -> [x])

seqLength :: Sequence a -> Int
seqLength = getSum . foldMap (\_ -> Sum 1)

-- Ex 3

instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty

-- Ex 4

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq0 = go [seq0]
  where
    go [] = False
    go (Empty   : ks) = go ks
    go (Single y : ks)
        | x == y    = True
        | otherwise = go ks
    go (Append l r : ks) = go (l : r : ks)

-- Ex 5

tailToList' :: Sequence a -> [a]
tailToList' seq0 = go [seq0] []
  where
    go [] acc = acc
    go (Empty : ks) acc = go ks acc
    go (Single x : ks) acc = go ks (x : acc)
    go (Append l r : ks) acc = go (r : l : ks) acc

-- Ex 6

data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (TNum n : ts) stack = go ts (n : stack)

    go (op : ts) (b : a : rest) =
        case op of
            TAdd -> go ts (a + b       : rest)
            TSub -> go ts (a - b       : rest)
            TMul -> go ts (a * b       : rest)
            TDiv -> if b == 0
                        then Nothing
                        else go ts (a `div` b : rest)
            TNum n -> go ts (n : b : a : rest)

    go _ _ = Nothing

-- Ex 7

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
        | p x = x : acc
        | otherwise = []

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- Ex 8

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : rest)
        | x == y    = (y, n + 1) : rest
        | otherwise = (x, 1) : (y, n) : rest

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []