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
-- Complete the 'findPoint' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER px
--  2. INTEGER py
--  3. INTEGER qx
--  4. INTEGER qy
--

findPoint px py qx qy = do
    [2*qx - px, 2*qy - py]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    forM_ [1..n] $ \n_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

        let px = read (firstMultipleInput !! 0) :: Int

        let py = read (firstMultipleInput !! 1) :: Int

        let qx = read (firstMultipleInput !! 2) :: Int

        let qy = read (firstMultipleInput !! 3) :: Int

        let result = findPoint px py qx qy

        hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr