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

utopianTree :: Int->Int
utopianTree n = Data.List.foldl (\acc x -> (if even x then acc + 1 else acc*2) ) 1 [1..n]

main = do
  let result = utopianTree 4


  print $ show result