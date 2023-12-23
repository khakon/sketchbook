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

odds :: [Int] -> [Int]
odds [] = []
odds [x] = []
odds (e1:e2:xs) = e1 : odds xs

rev :: [Int] -> [Int]
rev [] = []
rev l = Data.List.last l : (rev $ Data.List.init l)

countValues :: String -> Int -> Int -> Int
countValues list h v


main = do
    
    let result = rev1 [1..101]

    print $ show result
