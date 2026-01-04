pascaleTriangle :: Int->[Int]
pascaleTriangle 1 = [1,1]
pascaleTriangle n = pascaleTriangleInner $ pascaleTriangle $ n-1
    where
        pascaleTriangleInner [x] = [x]
        pascaleTriangleInner (1:y:xs) = 1 : (1+y) : pascaleTriangleInner (y:xs)
        pascaleTriangleInner (x:y:xs) = (x+y) : pascaleTriangleInner (y:xs)

pascaleTriangleAcc :: Int->[[Int]]->[[Int]]
pascaleTriangleAcc 1 acc = acc
pascaleTriangleAcc n (x:xs) = (pascaleTriangleAcc (n-1) ((pascaleTriangleInner x) : xs)) ++ (x:xs)
    where
        pascaleTriangleInner [x] = [x]
        pascaleTriangleInner (1:y:xs) = 1 : (1+y) : pascaleTriangleInner (y:xs)
        pascaleTriangleInner (x:y:xs) = (x+y) : pascaleTriangleInner (y:xs) 

pascaleTriangleN :: Int->[[Int]]->[[Int]]
pascaleTriangleN 0 acc = acc
pascaleTriangleN n acc = pascaleTriangle n : pascaleTriangleN (n-1) acc

reverseList :: [a]->[a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

main :: IO ()
main =
  putStr $ show $ reverseList $ pascaleTriangleN 4 [[1]]       

        
