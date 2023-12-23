{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

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

--
-- Complete the 'pageCount' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER p
--
pageCount :: Int -> Int -> Int
pageCount n p 
    | p `quot` 2 <= (n-p) `quot` 2 = p `quot` 2
    | (n `mod` 2) == 0 = (n - p + 1) `quot` 2
    | otherwise = (n-p) `quot` 2

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do

    let n = 2 :: Int

    let p = 1 :: Int

    let result = pageCount n p

    print $ show result

