module Lists where

import Control.Applicative
import Control.Monad
import Data.List

-- Problem 1
myLast :: [t] -> t
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [t] -> t
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: (Eq a, Num a) => [t] -> a -> t
elementAt (x:xs) n 
  | n == 1 = x
  | otherwise = elementAt xs (n - 1)

-- Problem 4
myLength :: Num a => [t] -> a
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: Num a => [t] -> a
myLength' xs0 = go xs0 0
  where go [] n = n
        go (_:xs) n = go xs (n + 1)
        
myLength'' :: [a] -> Integer
myLength'' = foldr ((+) . const 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [b] -> [b]
myReverse' = foldl (flip (:)) []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = liftM2 (==) id reverse

isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) <*> reverse

-- Problem 7
data List a = Elem a | List [List a] deriving Show

flatten :: List a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress (x:y:rest)
  | x == y = compress (y:rest)
  | otherwise = x : compress (y:rest)
                
compress x = x                

compress' :: Eq a => [a] -> [a]
compress' = foldr (\x acc -> if null acc || x /= head acc then x:acc else acc) []

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` head rest
              then (x:head rest) : tail rest
              else [x] : rest
  where rest = pack xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (liftM2 (,) length head) . group

-- Problem 11
data Code a = Multiple Int a | Single a deriving Show

encodeModified :: String -> [Code Char]
encodeModified = map go . encode
  where go (1, x) = Single x
        go (n, x) = Multiple n x
        
-- Problem 12        
decodeModified :: [Code Char] -> String        
decodeModified = concatMap go
  where go (Single x) = [x]
        go (Multiple n x) = replicate n x
        
-- Problem 12
valCode :: Code t -> t
valCode (Single x) = x
valCode (Multiple _ x) = x

incCode :: Code a -> Code a
incCode (Single x) = Multiple 2 x
incCode (Multiple n x) = Multiple (n + 1) x

encodeDirect :: Eq a => [a] -> [Code a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = if x == valCode c0
                      then incCode c0 : tail rest
                      else Single x : rest
  where rest = encodeDirect xs
        c0 = head rest

encodeDirect' :: String -> [Code Char]
encodeDirect' = foldr go []
  where go x [] = [Single x]
        go x xs@(y:ys) = if x == valCode y
                         then incCode y : ys
                         else Single x : xs

-- Problem 14
dupli :: [b] -> [b]
dupli = concatMap (replicate 2)

-- Problem 15
repli :: [b] -> Int -> [b]
repli = flip $ concatMap . replicate

-- Problem 16
dropEvery :: (Eq a, Num a) => [a1] -> a -> [a1]
dropEvery xs0 n0 = go xs0 n0
  where go (_:xs) 1 = go xs n0
        go (x:xs) n = x:go xs (n - 1)
        go [] _ = []
                     
dropEvery' :: (Enum b1, Eq b1, Num b1) => [b] -> b1 -> [b]
dropEvery' xs n = map fst . filter ((/= n) . snd) $ zip xs (cycle [1..n])

-- Problem 17
split :: (Eq a, Num a) => [a1] -> a -> ([a1], [a1])
split xs 0 = ([], xs)
split (x:xs) n = (x:x', xs')
  where (x', xs') = split xs (n - 1)

-- Problem 18
slice :: (Eq a2, Eq a, Num a2, Num a) => [a1] -> a -> a2 -> [a1]
slice xs i j = snd (split (fst (split xs j)) (i - 1))

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = t ++ h
  where (h, t) = split xs (n `mod` length xs)
        
-- Problem 20
removeAt :: (Eq a, Num a) => a -> [a1] -> (a1, [a1])
removeAt n xs0 = (x, x' ++ xs)
  where (x', x:xs) = split xs0 (n - 1)