import qualified Data.Text
import qualified Data.List
 --matrixRotation matrix r = do
    -- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    let firstMultipleInputTemp = "4 4 2"

    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let m = read (firstMultipleInput !! 0) :: Int

    let n = read (firstMultipleInput !! 1) :: Int

    let r = read (firstMultipleInput !! 2) :: Int

    let matrixTemp = ["1 2 3 4", "5 6 7 8", "9 10 11 12", "13 14 15 16"]; 

    let matrix = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) matrixTemp

    print $ matrix