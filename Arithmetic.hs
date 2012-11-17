module Arithmetic where

import           Data.List
import           Lists

-- Problem 31
isPrime :: Integer -> Bool
isPrime n0 = go . floor . sqrt . fromInteger $ n0
  where go 1 = True
        go n | n0 `rem` n == 0 = False
             | otherwise = go (n - 1)

primes = sieve [2..]
sieve (x:xs) = x : [p | p <- sieve xs, p `rem` x /= 0]

-- Problem 32
myGCD x 0 = abs x
myGCD x y | x < y = myGCD y x
          | otherwise = myGCD y (x `rem` y)

-- Problem 33
coprime x y = myGCD x y == 1

-- Problem 34
totient 1 = 1
totient n = length . filter (coprime n) $ [1..(n - 1)]

-- Problem 35
primeFactors n0 = go n0 primes
  where go 1 _ = []
        go n pall@(p:ps) | n `rem` p == 0 = p : go (n `div` p) pall
                         | otherwise= go n ps
-- Problem 36
primeFactorsMult = map (\(x, y) -> (y,x)) . encode . primeFactors

-- Problem 37
totient' 1 = 1
totient' n = foldl' (\acc (p, m) -> acc * (p - 1) * p ^ (m - 1)) 1 . primeFactorsMult $ n

-- Problem 39
primesR r1 r2 = takeWhile (<= r2) . dropWhile (<r1) $ primes

-- Problem 40
goldbach n = twoSum $ primesR 3 n
  where twoSum pns = go pns (reverse pns)
        go (x:xs) (y:ys) | x + y == n = (x, y)
                         | x + y > n = go (x:xs) ys
                         | x + y < n = go xs (y:ys)
