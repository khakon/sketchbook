f :: [Int] -> [Int]
f lst = [x | x<-lst, x `mod` 2 == 0]

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). f. map read. lines $ inputdata