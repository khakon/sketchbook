hexSymbol :: Int -> Char
hexSymbol n = hexSymbolInner n "0123456789abcdef" 0
  where
    hexSymbolInner _ [] _ = '0'
    hexSymbolInner n (x : xs) i
      | n == i = x
      | otherwise = hexSymbolInner n xs (i + 1)

intToHex :: Int -> String
intToHex n = intToHexInner n
  where
    intToHexInner n
      | n <= 16 = [hexSymbol n]
      | otherwise = hexSymbol (n `div` 16) : intToHexInner (n `mod` 16)

main :: IO ()
main =
  print $ intToHex 33
