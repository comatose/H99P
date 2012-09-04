module Arithmetic where

import Lists

isPrime :: Integer -> Bool
isPrime n0 = go . floor . sqrt . fromInteger $ n0
  where go 1 = True
        go n | n0 `rem` n == 0 = False
             | otherwise = go (n - 1)

primes = sieve [2..]
sieve (x:xs) = x : [p | p <- sieve xs, p `rem` x /= 0]

myGCD x 0 = abs x
myGCD x y | x < y = myGCD y x
          | otherwise = myGCD y (x `rem` y)
                        
coprime x y = myGCD x y == 1

totient n = length . filter (coprime n) $ [1..(n - 1)]

primeFactors n0 = go n0 primes
  where go 1 _ = []
        go n pall@(p:ps) | n `rem` p == 0 = p : go (n `div` p) pall
                         | otherwise= go n ps
        
primeFactorsMult = map (\(x, y) -> (y,x)) . encode . primeFactors
