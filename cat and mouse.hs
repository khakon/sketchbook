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


catAndMouse :: Int -> Int -> Int -> String
catAndMouse x y z
    | abs (z - y) > abs (z - x) = "Cat A"
    | abs (z - y) < abs (z - x) = "Cat B"
    | otherwise = "Mouse C"



main = do
    
    let result = catAndMouse 1 2 3

    print $ show result
