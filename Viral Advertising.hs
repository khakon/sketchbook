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

getShared :: Int -> Int
getShared n
  | n == 1 = 5
  | otherwise = div (getShared (n - 1)) 2 * 3

getLiked :: Int -> Int -> Int
getLiked n acc
  | n == 0 = acc
  | otherwise = getLiked (n - 1) acc + div (getShared n) 2

viralAdvertising :: Int -> Int
viralAdvertising n = getLiked n 0

main = do
  --let result = viralAdvertising 5
  let result = Data.List.foldr (+3) 0 [1,2,3] 

  print $ show result