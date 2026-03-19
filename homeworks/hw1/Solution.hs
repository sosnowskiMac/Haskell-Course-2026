{-# LANGUAGE BangPatterns #-}

-- Ex 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = n `elem` primesTo n

-- Ex 1

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- primesTo n, q <- primesTo n, p <= q, p + q == n]

-- Ex 2

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(x, y) | x <- xs, y <- xs, x < y, gcd x y == 1]

-- Ex 4

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = 
    let m = length a
        p = length (head a)
        n = length (head b)
    in [[ sum[ a !! i !! k * b !! k !! j | k <- [0..p-1] ]
        | j <- [0..n-1] ] 
        | i <- [0..m-1] ]

-- Ex 5

selections :: [a] -> [(a, [a])]
selections [] = []
selections (y:ys) = (y, ys) : [ (x, y:rest) | (x, rest) <- selections ys ]

-- I dont know if we can simply constraint as  to Eq, based on signature in hw
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [ x:ps | (x, rest) <- selections xs, ps <- permutations (k-1) rest ]

-- Ex 6

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | y < x = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- Ex 7

power :: Int -> Int -> Int
power b e = go b e 1
    where
        go _ 0 !acc = acc
        go base exp !acc = go base (exp - 1) (base * acc)

-- Ex 8

listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:ys) acc = let nextMax = max y acc 
                    in nextMax `seq` go ys nextMax

listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go xs x
  where
    go [] !acc = acc
    go (y:ys) !acc = go ys (max y acc)

-- Ex 9

primes :: [Int]
primes = sieve [2..]

isPrime2 :: Int -> Bool
isPrime2 a = a == head (dropWhile (< a) primes)

-- Ex 10

mean :: [Double] -> Double
mean (x:xs) = go x 1 xs
  where
    go sum acc [] = sum / acc
    go sum acc (x:xs) = go (sum+x) (acc+1) xs

mean2 :: [Double] -> Double
mean2 (x:xs) = go x 1 xs
  where
    go !sum !count [] = sum / count
    go !sum !count (x:xs) = go (sum+x) (count+1) xs

stats :: [Double] -> (Double, Double)
stats xs = go xs 0 0 0
  where
    go [] !sumX !sumX2 !count = 
      let m = sumX / count
          v = (sumX2 / count) - (m * m)
      in (m, v)
    go (y:ys) !sumX !sumX2 !count = go ys (sumX + y) (sumX2 + y*y) (count + 1)
  