module Lists where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Data.List
import System.Random hiding (split)

-- Problem 1
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt (x:xs) n 
  | n == 1 = x
  | otherwise = elementAt xs (n - 1)

-- Problem 4
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' xs0 = go xs0 0
  where go [] n = n
        go (_:xs) n = go xs (n + 1)
        
myLength'' = foldr ((+) . (const 1)) 0

-- Problem 5
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' = foldl (flip (:)) []

-- Problem 6
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
compress (x:y:rest)
  | x == y = compress (y:rest)
  | otherwise = x : compress (y:rest)
                
compress x = x                

compress' xs = foldr (\x acc -> if null acc || x /= head acc then x:acc else acc) [] xs

-- Problem 9
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head rest)
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
valCode (Single x) = x
valCode (Multiple _ x) = x

incCode (Single x) = Multiple 2 x
incCode (Multiple n x) = Multiple (n + 1) x

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
dupli = concatMap (replicate 2)

-- Problem 15
repli = flip $ concatMap . replicate

-- Problem 16
dropEvery xs0 n0 = go xs0 n0
  where go (x:xs) 1 = go xs n0
        go (x:xs) n = x:go xs (n - 1)
        go [] _ = []
                     
dropEvery' xs n = map fst . filter ((/= n) . snd) $ zip xs (cycle [1..n])

-- Problem 17
split xs 0 = ([], xs)
split (x:xs) n = (x:x', xs')
  where (x', xs') = split xs (n - 1)

-- Problem 18
slice xs i j = snd (split (fst (split xs j)) (i - 1))

-- Problem 19
rotate xs n = t ++ h
  where (h, t) = split xs (n `mod` length xs)
        
-- Problem 20
removeAt n xs0 = (x, x' ++ xs)
  where (x', (x:xs)) = split xs0 (n - 1)
        
-- Problem 21
insertAt e (xs) 1 = e:xs
insertAt e (x:xs) n = x : insertAt e xs (n - 1)

-- Problem 22
range i j 
  | i == j = [i]
  | otherwise = i : range (i + 1) j
             
-- Problem 23   
rnd_select xs n
  | n == 0 = return []
  | otherwise = do
    i <- randomRIO (1, length xs)
    rest <- rnd_select xs (n - 1)
    return $ (xs !! (i - 1)):rest
    
-- Problem 24
rnd_select' xs n
  | n == 0 = return []
  | otherwise = liftM2 (:) (randomRIO (1, length xs) >>= return . (xs !!) . (subtract 1)) (rnd_select' xs (n - 1))

-- Problem 25
diff_select i0 j = go i0 [1..j] >>= return . fst
  where go i xs 
          | i == 0 = return ([], xs)
          | otherwise = do
            ri <- randomRIO (1, length xs)
            let (c, cs) = removeAt ri xs
            (y, ys) <- go (i - 1) cs
            return (c:y, ys)

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n xall@(x:xs)
  | n == 1 = map (:[]) xall
--  | n == length xall = [xall]
--  | n > length xall = []
  | otherwise =  map (x:) (combinations (n - 1) xs) ++ combinations n xs
                 
-- Problem 27
groupN [] _ = [[]]
groupN (n:ns) xs = -- [p:ps | p <- combinations n xs, ps <- groupN ns (drop p xs)]
  do p <- combinations n xs
     ps <- groupN ns (drop p xs)
     return $ p:ps
  where 
        drop [] xs = xs
        drop (d:ds) xs = drop ds (delete d xs)

-- Problem 28
lsort :: [String] -> [String]
lsort = map snd . sort . map (liftM2 (,) length id)

lsort2 = sortBy (compare `on` length)

lfsort xs = map snd . sort $ map (\x -> (count x xs, x)) xs
count x0 = foldl' go 0
  where go acc x | x == x0 = acc + 1
                 | otherwise = acc
                               
frequency x = length . filter (==x)