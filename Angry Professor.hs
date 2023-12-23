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

angryProfessor :: Int->[Int]->String
angryProfessor k xs 
    | k > Data.List.length (Data.List.filter (\x -> x <= 0) xs) = "YES"
    | otherwise = "NO"

main = do
  let result = angryProfessor 3 [-1, -3, 4, 2]


  print $ show result