module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


magicSquare :: [[Int]] -> [[Int]]
magicSquare [] = []
magicSquare ([]:_) = []
magicSquare m = Data.List.map Data.List.head m  : (magicSquare (Data.List.map Data.List.tail m))

myLast :: [Int] -> Int
myLast (x:[]) = x
myLast (x:xs) = myLast xs

reverseArr :: [Int] -> [Int]
reverseArr [] = []
reverseArr (x:xs) = reverseArr xs ++ [x]

getMagicSquare = [
        [8, 1, 6, 3, 5, 7, 4, 9, 2],
        [4, 3, 8, 9, 5, 1, 2, 7, 6],
        [2, 9, 4, 7, 5, 3, 6, 1, 8],
        [6, 7, 2, 1, 5, 9, 8, 3, 4],
        [6, 1, 8, 7, 5, 3, 2, 9, 4],
        [8, 3, 4, 1, 5, 9, 6, 7, 2],
        [4, 9, 2, 3, 5, 7, 8, 1, 6],
        [2, 7, 6, 9, 5, 1, 4, 3, 8]
    ];

calculateCost :: [Int] -> [Int] -> [Int] -> Int
calculateCost [] [] acc = sum acc
calculateCost (x:xs) (y:square) acc = calculateCost xs square ((abs (y - x)) : acc)

calculateCosts :: [[Int]] -> Int
calculateCosts xs = Data.List.head $ sort [ (calculateCost (joinList xs) square []) | square <- getMagicSquare]

joinList :: [[Int]] -> [Int]
joinList [] = []
joinList (x:xs) = x ++ joinList xs

sortListLess :: [Int] -> Int -> [Int]
sortListLess [] _ = []
sortListLess (x:xs) value
    | x < value = x : sortListLess xs value
    | otherwise = sortListLess xs value

sortListMore :: [Int] -> Int -> [Int]
sortListMore [] _ = []
sortListMore (x:xs) value
    | x > value = x : sortListMore xs value
    | otherwise = sortListMore xs value
    
sortList :: [Int] -> [Int]   
sortList [] = []
sortList (x:xs) = sortList (sortListLess xs x) ++ [x] ++ sortList (sortListMore xs x)


main = do
    
    -- let result = [ Data.List.reverse x | x <- magicSquare [[8,3,4], [1,5,9], [6,7,2]]]

    -- let result = Data.List.map Data.List.tail [[8,3,4], [1,5,9], [6,7,2]]

    -- let result = calculateCosts [[5, 3, 4], [1, 5, 8], [6, 4, 2]] -- (joinList [[5, 3, 4], [1, 5, 8], [6, 4, 2]])  [8, 1, 6, 3, 5, 7, 4, 9, 2] []
    
    let result = sortList [5,7,1,9,11,4]
    
    print $ show result
