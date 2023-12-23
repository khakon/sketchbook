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

hurdleRace :: Int->[Int]->Int->Int
hurdleRace _ [] acc = acc
hurdleRace k (x:xs) acc
    | x - k > acc = hurdleRace k xs (x - k)
    | otherwise = hurdleRace k xs acc

main = do
  let result = hurdleRace 4 [1, 6, 3, 5, 2] 0


  print $ show result