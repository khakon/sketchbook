import Data.List
(//) :: Eq a => [a] -> a -> [a]
[] // y = []
(x : xs) // y =
  if (x == y)
    then xs
    else x : (xs // y)

quicksort :: (Ord a) => [[a]] -> [[a]]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

permutationsR :: Eq a => [a] -> [[a]]
permutationsR [] = [[]]
permutationsR l = [ x : ps | x <- l , ps <-  permutationsR ( l // x )]


permutationsDo :: Eq a => [a] -> [[a]]
permutationsDo [] = [[]]
permutationsDo l = do
    x <- l
    ps <- permutationsDo ( l // x )
    return (x : ps)

permutationsL :: Eq a => [a] -> [[a]]
permutationsL [] = [[]]
permutationsL l = l >>= (\x -> permutationsL ( l // x ) >>= (\ps -> return (x : ps)))

permutationsC :: Eq a => [a] -> [[a]]
permutationsC [] = [[]]
permutationsC l = concatMap (\x -> concatMap(\ps -> return (x : ps)) (permutationsC ( l // x ))) l 

listMatching :: [Int] -> [Int] -> Bool
listMatching [] [] = True
listMatching [] _ = False
listMatching _ [] = False
listMatching (x:xs) (y:ys)
    | (x == y) || (x == 0) = listMatching xs ys
    | otherwise = False

matchingCount :: [Int] -> [[Int]] -> Int -> Int -> Int
matchingCount _ [] _ acc = acc
matchingCount l (x:xs) i acc
    | listMatching l  x = matchingCount l xs (i + 1) (acc + i)
    | otherwise = matchingCount l xs (i + 1) acc

main = do
    let result = permutationsR [1..length[4, 3, 2, 1 ]]
    print $  mod (matchingCount [4, 3, 2, 1] result 1 0) (10^9 + 7)


-- Input (stdin)
-- 25
-- 22 15 14 0 5 0 9 12 0 0 0 0 0 0 0 3 17 20 0 0 0 0 0 0 23 22, 15, 14,0, 5, 0, 9, 12, 0, 0, 0, 0, 0, 0, 0, 3, 17, 20, 0, 0, 0, 0, 0, 0, 23
-- Expected Output
-- 792101048