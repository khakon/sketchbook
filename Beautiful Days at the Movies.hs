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

getInverse :: Int->Int->Int->Int->Int
getInverse divider n power result 
    | n < divider = result + n * (divider^power)
    | otherwise = getInverse divider (div n divider) (power - 1) (result + mod n divider * (divider^power)) 

reverseInt :: Int -> Int
reverseInt n = aux n 0
             where aux 0 y = y
                   aux x y = let (x',y') = x `quotRem` 10
                             in aux x' (10*y+y')
beautifulDays :: Int->Int->Int->Int
beautifulDays i j k = Data.List.foldl (\acc x -> (if mod (x - reverseInt x) k == 0 then acc + 1 else acc) ) 0 [i..j]                       

main = do
  let result = beautifulDays 20 23 6


  print $ show result