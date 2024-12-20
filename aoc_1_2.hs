import Data.Map (findWithDefault)
import qualified Data.Map as Map

listleft = [1, 2, 3, 4, 5]
listright = [5, 5, 6, 3, 2]

-- Function to read a file and convert its contents to a list of integers
parseFile :: FilePath -> IO [Int]
parseFile path = do
    contents <- readFile path
    let linesOfFiles = lines contents
        numbers = map read linesOfFiles :: [Int]
    return numbers

-- Function to update the map with a number
updateCount :: (Ord k, Num a) => k -> Map.Map k a -> Map.Map k a
updateCount key map = Map.insertWith (+) key 1 map

-- Function to build the counter map from a list of numbers
buildCounter :: (Ord k, Num a) => [k] -> Map.Map k a
buildCounter = foldr updateCount Map.empty


lookupCounter :: (Ord k, Num a) => k -> Map.Map k a -> a
lookupCounter n map = findWithDefault 0 n map

main = do
    lsl <- parseFile "aoc_1_1_input.txt"
    lsr <- parseFile "aoc_1_1_input2.txt"
    let counter = buildCounter lsr
    print (sum (zipWith (*) lsl (map (`lookupCounter` counter) lsl)))

-- 

--- >>> findWithDefault 0 2 (buildCounter listright)
-- 1
