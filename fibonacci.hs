fibonacciIter :: Int->Int->Int->Int
fibonacciIter a _ 0 = a
fibonacciIter a b counter = fibonacciIter b (a+b) (counter-1)

fibonacci :: Int->Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)


fibonacciList :: Int->[Int]
fibonacciList 0 = []
fibonacciList n = fibonacciIter 0 1 n : fibonacciList(n-1)

main :: IO ()
main =
  putStr $ show $ fibonacciList 5