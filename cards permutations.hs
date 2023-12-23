(//) :: Eq a => [a] -> a -> [a]
[] // y = []
(x : xs) // y =
  if (x == y)
    then xs
    else x : (xs // y)


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [ x : ps | x <- l , ps <-  permutations ( l // x )]

permutationsR :: Eq a => [a] -> [ [ a ] ]
permutationsR [] = [[]]
permutationsR (x:xs) = [x] : permutationsR ( xs // x )

permutationsDo :: Eq a => [a] -> [[a]]
permutationsDo [] = [[]]
permutationsDo l = do
    x <- l
    ps <- permutationsDo ( l // x )
    return (x : ps)

main = do
    print $ permutationsDo [1,2,3,4]
