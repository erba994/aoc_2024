{-# LANGUAGE BlockArguments #-}
import Data.Map (findWithDefault)
import qualified Data.Map as Map

listleft = [-1, -2, -3, -4, -5]
listright = [5, 5, 6, 3, 2]

-- Function to read a file with rows composed of numbers divided by space and convert its contents to a list of lists of integers
parseFile :: FilePath -> IO [[Int]]
parseFile path = do
    contents <- readFile path
    let linesOfFiles = lines contents
        numbers = map (map read . words) linesOfFiles
    return numbers


-- Function to update the map with a number
checkList :: (Ord a, Num t, Num a) => [a] -> a -> t
checkList (x:xs) m
    | null xs = 1
    | let diffNums = head xs - x, diffNums == 0 = 0
    | let diffNums = head xs - x, abs diffNums <= 3 && diffNums * m >= 0 = checkList xs diffNums
    | otherwise = 0

--- >>> checkList listleft 0
-- 1

main :: IO ()
main = do
    input_list <- parseFile "aoc_2_1_input.txt"
    print (sum (map checkList (input_list 0)))

-- 

--- >>> main
-- aoc_2_1_input.txt: openFile: does not exist (No such file or directory)
