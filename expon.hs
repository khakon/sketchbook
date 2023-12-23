{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

factorial :: Float->Float
factorial 1 = 1
factorial 0 = 1
factorial n = n * factorial (n-1)

expDegree :: Float->Float
expDegree x = sum[ (x**n)/(factorial n) | n<-[0..9]]

main :: IO()
main = do

    print $ show $ expDegree 20.0000

