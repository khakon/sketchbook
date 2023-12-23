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

cartProdSum :: [Int] -> [Int] -> [Int]
cartProdSum xs ys = [ x + y | x <- xs, y <- ys, x + y < 4]

getLast :: [Int] -> Int
getLast [] = -1
getLast xs = Data.List.last xs



main = do
    
    let result = getLast $ sort $ cartProdSum [1,2,3] [4,5]

    print $ show result
