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

getStepByLetter :: Char -> Int
getStepByLetter 'U' = 1
getStepByLetter 'D' = -1

countValues :: String -> Int -> Int -> Int
countValues [] _ v = v
countValues (x:xs) h v = countValues xs (h + getStepByLetter x) (if h == 0 && getStepByLetter x == -1 then v + 1 else v) 


main = do
    
    let result = countValues "UDDDUDUU" 0 0

    print $ show result
