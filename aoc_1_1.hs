import Data.Char (isDigit)
import Data.List (sort)
import Data.Sequence (elemIndexR)
import Data.Text (split)
import Data.Text.Internal.Read (digitToInt)
import System.IO ()


listleft = [1, 2, 3, 4, 5]
listright = [5, 5, 6, 3, 2]


sumListDistances :: (Num a, Ord a) => [a] -> [a] -> a
sumListDistances listl listr = sum $ map abs $ zipWith (-) (sort listl) (sort listr)
--- >>> sumListDistances listleft listright
-- 6

-- Function to read a file and convert its contents to a list of integers
parseFile :: FilePath -> IO [Int]
parseFile path = do
    contents <- readFile path
    let linesOfFiles = lines contents
        numbers = map read linesOfFiles :: [Int]
    return numbers


main :: IO()
main = do
    lsl <- parseFile "aoc_1_1_input.txt"
    lsr <- parseFile "aoc_1_1_input2.txt"
    print (sumListDistances lsl lsr)

--- >>> main
