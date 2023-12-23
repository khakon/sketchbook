module Main where

binaryDividers :: Int->[Int]->[Int]
binaryDividers n result
    | n < 2 = result++ [n]
    | otherwise = binaryDividers (div n 2) (result ++ [mod n 2]) 

eMulti :: Int->Int->Int->Int->Int
eMulti n a n1 a1
    | n == 1 && mod n1 2 == 0 = a
    | n == 1 && mod n1 2 == 1 = a + a1
    | otherwise = eMulti (div n 2) (a + a) n1 a1


main = do
    
    let result = eMulti 41 59 41 59
    print $ show result
