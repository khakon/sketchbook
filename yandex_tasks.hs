module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.List

isElement :: [Char]->Char->Bool
isElement [] _ = False
isElement (x:xs) e
    | x == e = True
    | otherwise = isElement xs e

jeweleryCount :: [Char]->[Char]->Int->Int
jeweleryCount _ [] acc = acc
jeweleryCount j (x:s) acc
    | isElement j x = jeweleryCount j s (acc + 1)
    | otherwise = jeweleryCount j s acc

countLetter :: [Char]->Char->Int->Int->Int
countLetter [] _ macc acc
    | macc > acc = macc
    | otherwise = acc
countLetter (x:xs) n macc acc
    | x == n = countLetter xs n (macc + 1) acc
    | x /= n && macc > acc = countLetter xs n 0 macc
    | otherwise = countLetter xs n 0 acc

searchSubstring :: String->String->String->Bool
searchSubstring _ [] _ = True
searchSubstring [] _ _ = False
searchSubstring (s:str) (ss:csubstr) substr
    | s == ss = searchSubstring str csubstr substr
    | otherwise = searchSubstring str substr substr


main :: IO ()
main = do
    --print $ show $ jeweleryCount "hytrew" "erts" 0
    --print $ show $ countLetter "hytttreoooosswssssssnnnnnyyyyysssssssssssssss" 's' 0 0
    print $ show $ searchSubstring "hytttreoooosswssssssnnnnnyyyyysssssssssssssss" "ooosswssssss" "ooosswssssss"