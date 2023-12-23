{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Data.Array
import Data.Bits
import Data.Foldable qualified as F
import Data.List qualified
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.List (permutations)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

testAppF :: Num a => a -> a -> a
testAppF = (+) <$> (* 2)

hexSymbol :: Int -> Char
hexSymbol n = hexSymbolInner n "0123456789abcdef" 0
  where
    hexSymbolInner _ [] _ = '0'
    hexSymbolInner n (x:xs) i
      | n == i = x
      | otherwise = hexSymbolInner n xs (i + 1)

intToHex :: Int -> String
intToHex n = intToHexInner n
  where
    intToHexInner n
      | n <= 16 = [hexSymbol n]
      | otherwise = hexSymbol (n `div` 16) : intToHexInner (n `mod` 16)


isPrime :: Int -> Bool
isPrime n = isPrime2 n 2
  where
    isPrime2 n i
      | i * i > n = True
      | mod n i == 0 = False
      | otherwise = isPrime2 n (i + 1)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Main.singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

money :: Int -> [Int] -> Int
money _ [] = 0
money total (m : moneys)
  | total - m == 0 = 1
  | total - m < 0 = 0
  | otherwise = money total moneys + money (total - m) (m : moneys)

moneyV :: Int -> [Int] -> [Int]
moneyV _ [] = []
moneyV total (m : moneys)
  | total - m == 0 = []
  | total - m > 0 = moneyV total moneys ++ moneyV (total - m) (m : moneys)
  | otherwise = []

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

mapC :: (a -> b) -> [a] -> [b]
mapC _ [] = []
mapC f (x : xs) = f x : mapC f xs

flatMap :: (a -> b) -> [a] -> [b]
flatMap _ [] = []
flatMap f xs = accumulateR (:) [] (mapC f xs)

filterC :: (a -> Bool) -> [a] -> [a]
filterC _ [] = []
filterC f (x : xs)
  | f x = x : filterC f xs
  | otherwise = filterC f xs

numberToArr :: Int -> [Int] -> [Int]
numberToArr c acc
  | c < 10 = c : acc
  | otherwise = numberToArr (div c 10) (mod c 10 : acc)

accumulateR :: (a -> b -> b) -> b -> [a] -> b
accumulateR _ initial [] = initial
accumulateR f initial (x : xs) = f x (accumulateR f initial xs)

accumulateL :: (b -> a -> b) -> b -> [a] -> b
accumulateL _ initial [] = initial
accumulateL f initial (x : xs) = f (accumulateL f initial xs) x

enumerateInterval :: Int -> Int -> [Int]
enumerateInterval low high
  | low > high = []
  | otherwise = low : enumerateInterval (low + 1) high

enumerateTree :: Int -> Int -> Tree Int
enumerateTree low high
  | low > high = EmptyTree
  | otherwise = treeInsert low (enumerateTree (low + 1) high)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- fib n = fibT 1 0 n

fibT :: Int -> Int -> Int -> Int
fibT _ b 0 = b
fibT a b n = fibT (a + b) a (n - 1)

prime :: Int -> Bool
prime n = prime2 n 2
  where
    prime2 n i
      | i * i > n = True
      | mod n i == 0 = False
      | otherwise = prime2 n (i + 1)

pairList :: Int -> [(Int, Int)]
-- pairList n = pairListInner [1 .. n]
--   where
--     pairListInner xs = Prelude.concat (Prelude.map (\x -> (Prelude.concat (Prelude.map (\y -> [(x,y)]) xs))) xs)
pairList n = pairListInner [1 .. n]
    where
        pairListInner [] = []
        pairListInner (x : xs) = Prelude.concat (Prelude.map (\y -> [(x, y)]) xs) ++ pairListInner xs

permutations :: Int -> [(Int, Int, Int)]
-- permutations n = Prelude.filter (\(x, y, z) -> x /= y && y /= z && x /= z) ((,,) <$> [1 .. n] <*> [1 .. n] <*> [1 .. n])
-- permutations n = do
--   x <- [1 .. n]
--   y <- [1 .. n]
--   z <- [1 .. n]
--   guard (x /= y && y /= z && x /= z)
--   return (x, y, z)
-- permutations n = permutationsInner [1 .. n]
--   where
--     permutationsInner [] = []
--     permutationsInner (x : xs) = Prelude.concat (Prelude.map (\y -> Prelude.concat (Prelude.map (\z -> [(x, y, z)]) xs)) xs) ++ permutationsInner xs
permutations n = permutationsInner [1 .. n]
    where
        permutationsInner xs = xs >>= (\x -> xs >>= (\y -> xs >>= (\z -> guard (x /= y && y /= z && x /= z) >> return (x, y, z)))) 

createPairs :: Int -> [(Int, Int)]
createPairs n = Prelude.filter (\(x, y) -> x > y && isPrime (x + y)) ((,) <$> [1 .. n] <*> [1 .. n])
-- createPairs n = [1 .. n] >>= (\x -> [1 .. n] >>= (\y -> guard (x > y && isPrime (x + y)) >> return (x, y)))
-- createPairs n = do
--   x <- [1 .. n]
--   y <- [1 .. n]
--   guard (x > y && isPrime (x + y))
--   return (x, y)
lm = ((\x ->(\x ->(\x -> x) 4) 3) 2)

main :: IO ()
main =
  print $ (testAppF 3) 4
-- print $ ((\x ->(\x ->(\x -> x) 4) 3) 2)
-- print $ intToHex 100
-- print $ Main.intToHex 32
-- print $ Main.permutations 3
-- print $ show $ money 3 [1,2]
-- print $ show $ mapC (*2) [1,5,10,25,50]
-- print $ show $ filterC (<20) . mapC (*2) $ [1,5,10,25,50]
-- print $ numberToArr 1000 []
-- print $ accumulateR (+) 0 [1, 2, 3, 4, 5]
-- print $ accumulateR (:) [] [1, 2, 3, 4, 5]
-- print $ enumerateInterval 1 7
-- print $ listLength $ mapC fib (enumerateInterval 1 7)
-- print $ accumulateL (\acc x -> acc ++ [x]) [] [1, 2, 3, 4, 5]
-- print $ enumerateTree 1 7
-- print $ filterC prime (enumerateInterval 1 100)
-- print $ accumulateL (accumulateR (:)) [] $ pairPrime 10
-- print $ filterC (\(x1:x2:x) -> prime(x1 + x2)) $ pairList 7
-- print $ permutations [1,2,3]
-- print $ mapC fib (enumerateInterval 1 7)
-- print $ show $ moneyV 100 [1,5,10,25,50]
-- print $ fmap (\x -> [x]) (\x -> 3*x) 5
-- print $ (\x -> [x]) <$> (*3) $ 5
-- let r = \x -> \x -> [x] ((\x -> 3*x) 5)
-- print $ (\_ -> \x -> [x]) <*> (\x -> [x]) $ 2
-- print $ (,,) <$> (1*) <*> (3+) <*> (5*) $ 5
-- print $ sequenceA [(+3),(+2),(+1)] 3
-- print $ Prelude.map (+3) [1,2,3]
-- print $ ((+3) <$> [1,2,3])
-- print $ ((+) <$> [1,2,3]) <*> [1,2,3]
-- print $ liftA2 (+) [1,2,3] [1,2,3]
-- print $ Data.List.foldr (++) [] [[1,2],[3,6],[9]]
