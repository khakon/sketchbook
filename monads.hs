

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

main :: IO()
main = print $ show $ Data.List.map (\x -> [x, x+1]) [10,20,30]