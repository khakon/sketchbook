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

sortM :: [Int]->[Int]
sortM [] = []
sortM (x:xs) = sortM(Data.List.filter (\xg -> xg < x) xs) ++ [x] ++ sortM(Data.List.filter (\xg -> xg >= x) xs)

main = do
  let result = sortM [-1, -3, 4, 2]


  print $ show result