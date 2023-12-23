import Data.Char
newtype Writer a = Writer (a, String)


instance Functor Writer where
  fmap f (Writer (x, s)) = Writer (f x, s)


upCase :: String -> Writer String
upCase s = Writer (map toUpper s, "upCase ")

toWords :: String -> Writer [String]
toWords s = Writer (words s, "toWords ")

compose :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
compose f g = \x -> let Writer (y, s1) = f x
                        Writer (z, s2) = g y
                    in Writer (z, s1 ++ s2)

processString :: String -> Writer[String]
processString = compose upCase toWords