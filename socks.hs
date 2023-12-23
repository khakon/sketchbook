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
-- Complete the 'sockMerchant' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER_ARRAY ar
--

sockMerchant n ar = sum [(Data.List.length x) `quot` 2 | x <- groupAr]

    -- Write your code here
    where groupAr = Data.List.group $ Data.List.sort $ ar

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do


    let n = 9 :: Int

    let ar = Data.List.map (read :: String -> Int) . Data.List.words $ "10 20 20 10 10 30 50 10 20"

    let result = sockMerchant n ar :: Int

    putStrLn $ show result

