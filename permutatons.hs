import Control.Monad

filterByList :: [Int] -> [Int] -> [Int]
filterByList xs [] = xs
filterByList xs (y : ys) = filterByList (filter (/= y) xs) ys

permutatiomItem :: Int -> [Int] -> [[Int]] -> [[Int]]
permutatiomItem 0 _ acc = acc
permutatiomItem n xs acc = (concat $ map (\x -> map (\y -> y : x) (filterByList xs x)) acc) ++ permutatiomItem (n - 1) xs (concat $ map (\x -> map (\y -> y : x) (filterByList xs x)) acc)

permutationsList :: [[Int]]
permutationsList = [[x, y, z] | x <- [1, 2, 3], y <- [1, 2, 3], y /= x, z <- [1, 2, 3], z /= x && z /= y]

permutationsMonad = [1, 2, 3] >>= \x -> filter (/= x) [1, 2, 3] >>= \y -> filter (\z -> z /= x && z /= y) [1, 2, 3] >>= \z -> return [x, y, z]

permutationsMonadGuard = [1, 2, 3] >>= \x -> [1, 2, 3] >>= \y -> guard (y /= x) >> [1, 2, 3] >>= \z -> guard(z /= x && z /= y) >> return [x, y, z]

main :: IO ()
main =
  putStr $ show $ permutationsMonadGuard

-- permutatiomItem 3 [1,2,3] [[1]]