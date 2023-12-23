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

pickingNumbers :: [Int]->[Int]->Int->Int
pickingNumbers [] _ acc = acc
pickingNumbers (x:xs) ys acc = pickingNumbers xs ys (getAcc x ys acc 0)

getAcc :: Int->[Int]->Int->Int->Int
getAcc _ [] oldAcc newAcc
    | oldAcc >= newAcc = oldAcc
    | otherwise = newAcc
getAcc el (x:xs) oldAcc newAcc
    | x-el == 0 = getAcc el xs oldAcc (newAcc+1)
    | x-el == 1 = getAcc el xs oldAcc (newAcc+1)
    | otherwise = getAcc el xs oldAcc newAcc

main = do
    
    let result = pickingNumbers [4, 6, 5, 3, 3, 1] [4, 6, 5, 3, 3, 1] 0
    
    print $ show result