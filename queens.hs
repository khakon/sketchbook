queensFilterItem :: (Int, Int)->[(Int, Int)]->Bool
queensFilterItem _ [] = True
queensFilterItem (xc,xr) ((yc,yr):ys) 
    | xr /= yr && abs(xr - yr) /= abs(xc - yc) = queensFilterItem (xc,xr) ys
    | otherwise = False

queens :: Int->[[(Int, Int)]]
queens n = queenCols n
    where
        queenCols 0 = [[]]
        queenCols n = [ (n, newRow) : restQueens | restQueens <- queenCols (n-1), newRow <- [1..4], queensFilterItem (n, newRow) restQueens]

printQueensItem :: [(Int, Int)] -> String
printQueensItem [] = ""
printQueensItem ((xc,xr):xs) = [if indx == xr then 'Q' else '.' | indx <- [1..4]] ++ "\n" ++ printQueensItem xs

printQueens :: [[(Int, Int)]] -> String
printQueens [] = ""
printQueens (x:xs) = show x ++ "\n" ++ "----------------" ++ "\n" ++ printQueensItem x ++ "\n" ++ "----------------" ++ "\n" ++ printQueens xs

main :: IO ()
main =
  putStr $ printQueens $ queens 4