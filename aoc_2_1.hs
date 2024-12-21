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

--- >>> parseFile "aoc_2_1_input.txt"
-- [[14,17,20,21,24,26,27,24],[39,41,43,45,46,46],[35,38,39,41,44,47,50,54],[68,69,71,74,75,78,80,87],[80,82,81,82,83,85,88],[48,51,54,55,58,57,55],[41,44,47,50,47,47],[66,68,71,70,73,77],[29,32,29,30,35],[77,78,79,81,83,83,86,88],[10,13,14,16,19,19,20,17],[61,64,65,67,67,67],[29,30,31,32,35,35,39],[24,25,25,28,31,38],[61,64,65,66,70,73,76],[35,37,39,43,40],[41,42,43,47,48,49,49],[3,5,9,11,15],[24,26,28,32,34,35,40],[48,49,50,57,59,62,64,67],[18,20,22,25,26,32,35,33],[76,79,85,86,86],[55,58,59,66,70],[9,11,13,14,20,23,29],[58,56,58,59,60,62,63,65],[42,39,40,41,42,43,46,43],[83,80,81,83,86,89,89],[48,47,48,50,52,56],[38,37,38,39,46],[54,53,56,53,55,56],[79,76,74,77,76],[68,65,63,64,67,70,70],[84,81,83,86,84,87,91],[72,71,74,72,78],[38,35,36,36,38,41,43,46],[45,43,45,47,49,49,50,48],[23,22,24,24,24],[9,6,8,10,12,12,13,17],[77,75,78,78,79,80,83,89],[60,58,62,65,66,69,71],[64,62,63,64,68,67],[76,73,76,77,81,83,83],[21,19,21,25,26,29,31,35],[53,52,53,54,58,61,63,70],[91,89,94,97,99],[15,14,17,23,24,25,22],[65,62,65,71,74,75,75],[79,76,77,83,87],[12,10,15,18,21,23,29],[63,63,64,65,67,70,72],[51,51,53,56,58,56],[85,85,86,89,91,93,93],[16,16,17,19,20,23,25,29],[64,64,67,68,71,77],[2,2,5,8,11,10,13],[38,38,37,39,41,39],[77,77,74,77,78,80,80],[16,16,18,20,23,21,25],[79,79,82,83,85,84,86,92],[86,86,88,88,91,92,93,94],[93,93,95,95,92],[14,14,14,17,18,18],[58,58,59,59,63],[82,82,84,85,85,87,94],[72,72,76,79,82,84],[89,89,93,96,95],[6,6,10,11,11],[55,55,59,62,66],[53,53,57,58,63],[28,28,35,36,39,41,43,46],[22,22,25,27,28,33,30],[1,1,2,9,9],[83,83,89,90,91,95],[12,12,19,20,26],[43,47,48,50,51,52],[52,56,57,59,61,63,65,63],[47,51,53,54,57,57],[83,87,88,89,91,92,96],[75,79,81,83,84,91],[43,47,44,46,48],[19,23,21,22,25,26,29,27],[4,8,10,7,10,12,15,15],[72,76,77,75,76,77,81],[49,53,51,54,61],[36,40,40,42,45,47,49],[64,68,69,69,66],[24,28,29,29,30,32,34,34],[77,81,82,82,85,86,90],[36,40,40,42,45,51],[13,17,18,19,22,26,27],[74,78,81,85,88,86],[81,85,86,90,92,92],[2,6,8,12,16],[68,72,74,78,85],[30,34,35,42,45,47],[87,91,96,98,99,96],[48,52,57,60,62,65,65],[20,24,25,27,34,35,39],[68,72,75,78,84,85,92],[4,9,12,14,15,18,20],[33,40,41,44,47,48,46],[30,35,36,37,40,42,45,45],[75,80,81,82,85,87,88,92],[35,40,43,45,48,54],[55,61,58,59,62],[23,30,27,28,30,27],[57,63,62,65,65],[51,56,59,56,59,63],[53,60,62,65,62,67],[14,19,19,21,24,27,28],[52,58,58,60,61,64,66,64],[61,66,67,68,71,71,73,73],[12,19,22,24,26,29,29,33],[47,53,55,58,58,64],[63,70,72,75,77,81,84],[7,14,16,20,21,18],[20,26,29,30,34,36,36],[7,13,15,17,21,25],[70,77,80,81,84,85,89,94],[13,20,26,27,29,32,33],[68,74,75,81,78],[71,76,82,84,86,86],[50,55,58,60,65,67,70,74],[9,15,17,24,25,30],[35,33,32,30,27,30],[22,20,17,14,11,8,5,5],[97,96,94,92,91,90,88,84],[23,22,21,20,18,16,14,7],[16,15,14,16,15],[35,34,31,29,26,28,29],[41,39,37,35,33,31,32,32],[88,85,83,86,82],[38,36,34,32,34,29],[12,9,9,7,5],[93,92,89,89,91],[82,81,79,76,76,75,75],[31,29,29,26,25,24,23,19],[74,72,71,71,69,67,62],[28,27,23,22,21,20,19,17],[56,53,49,47,44,41,43],[60,59,55,53,50,50],[62,60,57,55,53,50,46,42],[59,58,55,51,45],[26,23,20,13,12,10,9],[20,19,16,15,14,7,9],[19,17,16,14,11,6,5,5],[98,95,89,87,85,81],[81,80,79,73,70,64],[68,71,68,65,63,60,59,56],[72,74,72,71,68,65,64,66],[74,77,76,74,72,72],[59,60,59,57,54,50],[36,38,37,36,33,30,27,21],[69,72,69,70,67,64],[17,18,16,13,14,13,16],[14,16,19,17,15,14,14],[38,40,42,41,37],[44,46,48,46,40],[84,85,83,83,80,79],[34,36,36,33,32,31,34],[51,52,49,47,47,44,44],[83,84,81,81,79,78,74],[81,84,84,83,82,75],[94,96,95,91,90,88],[59,62,61,59,55,53,56],[20,23,20,18,14,11,11],[59,62,58,55,52,48],[27,29,28,24,17],[96,97,96,91,90,87],[10,13,7,5,3,1,4],[88,90,84,82,80,79,79],[39,41,38,31,29,25],[74,77,72,71,68,61],[40,40,37,34,32],[91,91,89,88,87,90],[90,90,89,87,87],[95,95,94,92,90,88,87,83],[60,60,59,57,52],[85,85,82,80,79,77,80,77],[52,52,49,46,45,48,49],[6,6,4,5,5],[35,35,34,35,32,31,27],[68,68,65,64,63,60,62,57],[89,89,86,84,84,81],[38,38,38,36,33,35],[9,9,8,5,2,2,2],[51,51,51,50,46],[59,59,59,57,50],[62,62,61,57,55,52,49],[71,71,70,67,66,63,59,60],[32,32,28,26,26],[64,64,62,60,56,54,53,49],[89,89,87,85,84,80,75],[76,76,74,68,65,64],[48,48,41,38,37,35,33,36],[43,43,40,38,35,33,26,26],[60,60,59,58,51,47],[65,65,60,59,57,55,54,48],[21,17,14,12,9],[27,23,21,19,20],[34,30,27,26,23,21,19,19],[28,24,23,20,18,16,13,9],[60,56,55,53,51,50,47,42],[70,66,63,60,61,58,55],[76,72,69,72,71,72],[80,76,74,75,74,73,73],[69,65,62,65,62,61,59,55],[17,13,16,15,14,9],[26,22,21,18,17,17,15],[84,80,80,77,74,77],[67,63,62,60,60,59,56,56],[31,27,27,25,22,18],[93,89,86,86,81],[76,72,71,69,68,64,63],[29,25,21,20,23],[65,61,57,54,54],[23,19,16,12,9,8,5,1],[62,58,55,53,51,47,46,40],[25,21,14,11,10,7],[30,26,21,19,20],[55,51,50,43,42,39,39],[60,56,54,52,51,50,44,40],[44,40,38,35,32,29,22,15],[70,63,62,60,57,55],[95,89,88,85,88],[43,38,37,35,35],[85,80,77,75,74,73,69],[52,45,43,41,38,37,30],[87,82,79,81,78],[38,32,29,31,30,32],[63,58,57,60,58,57,57],[56,49,51,49,45],[43,38,39,37,35,33,32,25],[28,21,21,20,17,16,15,13],[73,68,68,65,63,66],[55,48,46,46,45,45],[20,14,14,11,8,4],[23,17,17,16,10],[17,10,8,4,2],[85,80,78,75,73,72,68,70],[52,45,44,43,40,39,35,35],[87,80,78,74,72,69,66,62],[68,63,59,58,52],[29,22,17,15,14,12],[53,47,42,41,44],[99,92,89,87,80,77,75,75],[90,85,82,75,71],[84,78,76,73,71,70,65,58],[12,15,16,18,21,22,23,20],[53,56,57,60,62,63,63],[20,23,26,27,29,30,34],[27,29,32,35,38,40,43,49],[86,89,91,93,95,93,94],[54,56,57,56,59,57],[87,90,88,90,93,96,97,97],[46,47,48,47,51],[89,90,87,89,91,93,99],[18,21,24,25,27,27,30,32],[77,79,81,81,82,80],[43,45,46,48,50,50,50],[84,85,88,88,92],[62,63,63,66,71],[51,53,57,59,62,65],[12,15,18,22,24,25,24],[83,85,88,92,95,95],[3,6,8,12,16],[9,11,13,15,19,26],[80,82,85,86,91,92],[6,7,9,14,17,15],[67,68,70,73,74,81,81],[16,19,20,22,28,32],[78,80,86,89,95],[11,10,12,15,17,20,22],[26,25,27,28,30,29],[74,73,74,77,78,78],[72,71,74,75,76,80],[49,46,49,52,55,60],[53,50,52,50,52,54],[42,40,41,44,43,41],[6,5,7,8,9,11,9,9],[51,50,53,54,55,54,58],[73,72,70,72,75,78,84],[74,73,75,78,80,80,82],[95,94,95,97,97,94],[5,2,2,5,7,7],[75,74,74,75,76,80],[91,89,89,92,99],[11,9,10,14,15,18,19,21],[47,44,48,50,51,50],[42,40,42,44,45,47,51,51],[45,42,46,48,52],[57,56,60,61,63,69],[79,76,79,80,82,87,90],[23,21,22,25,32,33,35,34],[81,80,85,87,87],[33,32,35,36,39,44,47,51],[80,77,78,83,90],[2,2,5,8,11,13],[57,57,60,61,62,65,62],[56,56,57,59,61,64,65,65],[77,77,78,80,83,84,88],[5,5,7,8,11,17],[13,13,14,16,13,14],[88,88,87,88,86],[24,24,25,28,30,29,29],[43,43,40,42,46],[37,37,38,39,38,45],[87,87,88,89,91,91,93],[74,74,75,77,78,78,77],[85,85,86,87,87,90,90],[29,29,29,32,36],[72,72,72,73,74,77,82],[73,73,76,80,82,83,85,86],[3,3,4,8,10,7],[13,13,15,19,21,24,24],[62,62,64,67,71,73,77],[75,75,77,80,84,85,88,95],[8,8,11,16,18,21],[31,31,34,37,38,44,43],[53,53,54,56,59,64,65,65],[30,30,32,33,36,42,46],[6,6,9,16,21],[75,79,80,82,84],[20,24,25,26,24],[35,39,41,44,47,49,49],[46,50,52,55,56,60],[84,88,91,93,99],[60,64,66,63,64],[71,75,76,79,78,81,80],[30,34,31,32,32],[64,68,66,68,71,75],[43,47,50,51,50,51,58],[64,68,68,70,71],[54,58,58,59,62,61],[41,45,45,48,48],[9,13,14,17,17,19,23],[61,65,67,67,70,72,74,81],[2,6,7,11,13,15],[30,34,37,41,39],[68,72,76,79,82,84,84],[5,9,10,13,17,21],[65,69,70,74,80],[77,81,86,87,89,90,91,94],[59,63,66,73,72],[73,77,80,81,86,86],[18,22,23,25,30,33,37],[35,39,44,46,47,52],[16,22,23,26,29,32,35,37],[67,73,76,79,81,84,87,86],[1,8,10,12,14,14],[30,36,37,39,40,41,42,46],[36,41,43,44,47,49,54],[49,54,57,56,58,59,62,65],[48,53,56,58,60,58,60,57],[22,27,28,27,28,31,34,34],[11,16,19,20,21,18,21,25],[36,41,42,40,47],[4,10,13,13,15],[13,18,20,21,23,23,20],[51,58,61,61,62,62],[18,25,28,29,29,31,33,37],[59,65,68,71,71,73,76,81],[75,80,81,82,86,87],[11,18,21,24,25,29,32,29],[27,34,37,41,44,44],[7,12,16,17,18,22],[25,30,32,36,39,45],[74,80,87,89,92],[54,59,62,68,71,68],[2,7,9,11,13,20,20],[41,46,48,53,55,58,62],[67,74,79,81,87],[66,64,63,62,64],[51,50,48,47,45,45],[21,18,16,15,11],[30,28,26,24,23,22,17],[45,44,41,40,41,40,37],[64,61,62,61,60,57,56,59],[58,56,55,56,54,54],[11,9,12,11,7],[79,76,73,75,70],[35,33,30,28,28,26],[31,28,25,25,24,25],[56,55,54,54,53,53],[46,44,41,38,35,35,31],[88,87,87,86,80],[12,11,7,4,1],[39,38,37,35,31,33],[17,15,14,12,9,5,4,4],[21,20,17,13,10,7,5,1],[43,41,40,39,35,30],[73,70,67,66,59,57,54,52],[60,57,51,48,45,48],[22,21,19,12,11,9,9],[90,89,87,85,83,78,74],[28,26,24,18,15,12,11,6],[18,20,17,16,15,12,9,6],[80,83,80,77,76,77],[86,89,86,83,83],[47,49,46,45,41],[35,38,37,34,28],[8,9,12,11,9,6],[80,83,81,82,85],[17,18,17,14,16,15,15],[61,63,60,58,57,60,59,55],[60,62,61,63,57],[47,49,49,46,45,42,39,36],[36,38,38,36,34,33,32,34],[66,69,66,63,61,58,58,58],[78,81,81,79,77,76,72],[70,72,71,69,68,68,61],[60,63,60,59,56,52,49],[88,91,87,86,85,86],[90,92,90,88,84,82,82],[36,38,34,33,29],[45,47,43,41,38,37,36,31],[83,86,84,81,78,72,70,69],[48,49,44,41,43],[82,83,82,80,77,72,72],[93,94,92,89,86,79,75],[75,77,74,68,67,64,62,56],[57,57,54,51,48,47,46,45],[96,96,93,92,91,88,90],[35,35,32,29,26,23,20,20],[29,29,28,25,24,20],[61,61,60,58,52],[69,69,66,65,67,64],[11,11,8,10,7,4,3,5],[14,14,13,16,16],[23,23,24,22,18],[47,47,46,48,41],[12,12,12,10,9,6,4,3],[17,17,16,16,19],[25,25,25,22,19,17,16,16],[30,30,29,27,27,25,24,20],[98,98,98,97,94,87],[83,83,81,78,75,71,69,66],[48,48,44,43,46],[56,56,52,51,49,48,48],[57,57,53,51,47],[53,53,49,46,45,43,41,36],[94,94,91,86,85,83,82,81],[79,79,72,70,72],[83,83,78,76,75,72,72],[21,21,14,11,8,4],[90,90,88,85,78,71],[39,35,33,31,30,28,26],[21,17,14,13,11,13],[69,65,62,61,58,58],[67,63,61,59,57,55,51],[48,44,43,41,39,34],[95,91,90,91,89,88,87],[33,29,26,23,25,22,24],[88,84,87,85,84,83,81,81],[27,23,26,25,22,18],[91,87,86,83,84,81,76],[60,56,54,51,49,49,48,46],[81,77,77,74,77],[92,88,88,87,87],[91,87,85,84,82,82,80,76],[43,39,38,38,36,34,32,26],[45,41,37,36,35,32],[65,61,60,59,57,53,51,54],[99,95,91,90,88,85,85],[53,49,46,42,38],[68,64,62,61,57,56,54,47],[56,52,50,47,40,39],[30,26,24,22,16,13,15],[37,33,31,25,23,20,20],[72,68,62,61,60,56],[46,42,39,34,28],[55,48,46,43,42,39,36],[34,27,26,24,22,21,20,21],[24,18,16,14,13,12,12],[72,65,63,61,59,55],[53,48,47,44,42,37],[51,46,44,42,39,42,40,38],[27,21,18,16,17,18],[95,90,88,89,89],[89,83,82,80,77,80,77,73],[75,68,70,69,66,61],[45,39,36,36,35],[44,39,37,35,35,37],[35,30,29,28,28,28],[46,40,37,36,34,33,33,29],[52,45,42,41,40,40,34],[85,80,79,78,74,73,71,69],[38,33,32,28,26,29],[78,73,72,69,65,65],[26,19,17,16,12,8],[24,18,14,13,6],[79,73,71,68,63,61],[88,81,74,72,69,68,70],[78,73,70,64,64],[82,75,68,65,61],[47,40,39,36,29,23],[20,21,22,25,24],[53,54,55,56,59,60,61,61],[4,7,8,9,11,14,18],[25,26,29,30,33,39],[14,17,20,21,23,25,24,27],[54,56,57,60,58,59,58],[73,76,77,74,74],[22,25,28,27,30,33,37],[68,69,70,72,69,76],[20,22,25,28,29,32,32,34],[83,86,87,87,89,87],[19,22,24,24,24],[52,53,53,56,59,61,65],[62,64,67,69,69,76],[82,85,86,90,92,93],[9,10,12,16,17,20,19],[72,74,77,79,83,86,86],[44,47,48,49,50,54,57,61],[18,20,24,26,27,33],[86,87,89,94,95,96],[21,24,26,29,35,34],[10,13,14,16,18,19,25,25],[25,28,30,33,36,42,45,49],[58,61,63,70,76],[67,64,66,68,71,74,77,78],[21,19,22,24,25,26,24],[66,64,67,69,69],[90,87,90,93,94,98],[66,65,68,69,70,73,80],[14,12,9,12,15,18,20],[45,42,43,46,48,45,48,47],[97,94,93,95,97,97],[85,83,84,85,87,84,88],[55,54,56,53,54,59],[90,88,89,89,92,95,96,97],[3,2,3,4,4,7,9,6],[5,3,6,6,6],[40,39,41,44,46,46,50],[20,19,19,22,25,28,34],[83,81,83,87,88,89],[40,37,39,41,43,45,49,46],[77,75,79,81,83,83],[10,7,9,12,13,17,21],[70,69,72,74,76,80,87],[58,57,60,67,70,73,74],[73,72,74,81,80],[85,84,85,86,91,91],[44,42,44,50,52,54,55,59],[3,2,4,6,12,18],[84,84,85,86,88,89,90],[11,11,14,17,18,17],[29,29,30,33,36,38,40,40],[80,80,83,85,89],[32,32,34,36,39,42,49],[21,21,23,26,27,28,25,26],[95,95,94,97,96],[5,5,7,8,6,6],[73,73,74,73,77],[78,78,81,82,85,82,87],[88,88,91,94,94,95,97,98],[19,19,21,21,23,24,23],[54,54,54,55,58,58],[38,38,39,41,42,42,44,48],[53,53,53,56,59,61,66],[42,42,46,49,52,53],[60,60,64,67,69,66],[56,56,60,61,64,67,67],[58,58,60,64,65,68,69,73],[64,64,65,69,75],[77,77,80,85,88],[1,1,3,5,8,9,15,12],[23,23,24,26,33,36,37,37],[38,38,39,42,45,50,54],[45,45,47,48,50,57,62],[38,42,44,45,47,48],[76,80,81,84,85,83],[87,91,94,96,98,98],[9,13,14,17,18,21,25],[34,38,39,41,48],[19,23,25,28,27,28,29,31],[25,29,30,32,33,32,31],[82,86,89,86,86],[8,12,15,16,18,20,17,21],[54,58,59,62,65,63,68],[5,9,11,11,12,14,16],[35,39,41,43,46,46,47,46],[86,90,90,93,93],[53,57,58,58,59,62,66],[9,13,16,19,19,26],[49,53,57,59,62,63],[39,43,47,48,50,51,48],[22,26,27,29,32,36,36],[34,38,41,43,47,50,53,57],[65,69,72,73,77,82],[10,14,16,21,22,23,26,28],[17,21,22,24,29,26],[63,67,68,73,76,76],[28,32,35,41,45],[58,62,65,67,68,74,80],[15,22,23,26,28,29],[48,55,58,59,57],[57,64,66,68,69,69],[70,76,78,80,83,84,87,91],[46,51,54,55,57,59,62,68],[8,15,18,20,23,20,21,24],[81,86,88,86,89,88],[70,76,77,79,77,77],[45,52,54,51,55],[56,62,65,62,67],[62,69,71,71,74],[45,50,53,54,54,52],[63,69,71,71,72,72],[15,22,22,23,24,25,28,32],[30,36,39,42,42,48],[16,22,24,25,28,32,35],[14,21,25,28,29,30,33,31],[77,83,85,86,89,93,95,95],[32,37,41,43,45,46,50],[71,78,80,83,87,89,95],[63,69,72,78,79,80,82,84],[59,65,66,68,75,78,76],[13,18,23,25,26,26],[47,54,55,57,64,68],[52,58,63,65,68,73],[17,16,13,11,9,7,10],[12,10,9,8,6,5,5],[61,58,56,53,52,50,46],[72,71,70,69,66,65,59],[85,83,80,78,76,78,76,75],[27,24,25,24,21,18,16,19],[15,12,9,8,10,7,7],[37,34,31,29,31,28,26,22],[22,20,17,15,12,15,12,5],[76,73,71,70,70,67,64,61],[41,38,37,36,33,33,35],[96,94,91,89,87,87,87],[79,76,76,75,72,71,69,65],[59,56,53,53,50,45],[86,84,83,81,77,75,72,70],[32,29,28,24,22,24],[18,17,16,15,11,10,7,7],[18,16,15,11,7],[78,77,75,72,68,63],[35,32,26,23,20,19],[75,74,72,70,68,63,60,62],[38,37,36,29,26,23,23],[50,49,48,47,46,40,39,35],[77,75,72,67,65,58],[83,84,82,80,78,76,73],[34,37,35,33,30,27,26,27],[38,39,36,35,35],[79,81,79,76,72],[63,66,65,63,61,55],[96,97,98,97,96,94],[37,40,43,40,38,41],[77,78,77,74,72,74,74],[79,82,83,81,80,79,75],[14,15,14,13,12,15,14,9],[91,92,92,89,86],[39,42,42,41,39,36,37],[62,65,65,64,64],[41,44,44,43,40,36],[53,56,56,55,50],[25,26,24,21,18,14,13],[48,51,50,47,43,40,42],[11,13,10,9,5,5],[63,65,63,62,60,57,53,49],[53,55,51,50,44],[14,15,12,10,9,4,3],[86,89,82,81,78,76,73,76],[14,16,13,12,6,5,5],[17,18,13,11,7],[77,78,71,68,66,61],[55,55,54,51,50,47,44],[65,65,64,61,58,61],[66,66,64,63,61,59,59],[44,44,41,38,34],[48,48,45,43,42,37],[30,30,32,30,28],[86,86,84,81,78,81,82],[94,94,91,88,85,87,87],[71,71,70,69,68,67,68,64],[82,82,81,78,80,77,72],[42,42,39,39,37],[80,80,80,78,76,78],[69,69,66,65,65,65],[22,22,22,20,16],[17,17,15,15,14,8],[99,99,96,94,91,87,85,83],[21,21,17,15,16],[83,83,79,78,78],[21,21,18,16,12,9,5],[46,46,44,40,38,37,30],[26,26,24,19,18],[81,81,79,73,76],[51,51,45,44,44],[68,68,61,59,57,55,52,48],[20,20,19,18,17,12,5],[34,30,29,28,26,23,20,18],[27,23,22,21,18,16,18],[97,93,91,88,87,87],[17,13,10,9,8,6,2],[20,16,14,12,5],[96,92,90,87,89,87],[90,86,84,86,89],[45,41,42,39,37,36,36],[43,39,37,40,39,38,37,33],[98,94,91,90,88,86,88,82],[25,21,20,17,14,14,13],[91,87,87,86,85,87],[53,49,49,46,44,41,41],[86,82,82,79,78,75,73,69],[19,15,14,14,7],[85,81,80,76,73],[40,36,34,30,27,26,28],[61,57,54,53,49,48,46,46],[59,55,51,48,44],[78,74,70,67,66,65,59],[37,33,30,28,27,25,18,17],[17,13,6,5,2,4],[95,91,88,82,82],[71,67,64,61,58,52,48],[63,59,53,51,49,43],[62,56,54,53,52,49],[24,17,15,12,10,8,11],[66,60,57,56,55,53,53],[19,12,11,10,7,3],[64,59,57,54,52,46],[95,89,86,83,81,82,81,78],[93,86,85,83,86,84,86],[33,27,26,27,27],[98,92,93,91,90,89,88,84],[73,66,65,67,66,63,56],[67,61,60,60,58,56,53],[29,23,20,20,21],[86,80,78,78,78],[79,73,73,70,67,66,63,59],[81,76,75,72,72,65],[54,48,44,41,39],[47,40,39,37,33,35],[70,63,59,58,55,55],[44,39,35,32,28],[90,83,79,78,72],[42,37,34,32,29,27,21,19],[93,87,81,80,78,75,76],[93,86,83,76,74,74],[75,69,63,61,57],[79,73,72,70,68,62,55],[99,96,93,90,89,87,90,88],[58,60,57,56,53,50,46,40],[69,65,64,61,59,56,49],[36,39,42,45,45,47,45],[57,56,59,58,57,54,51,45],[52,55,56,53,54,59],[34,35,33,30,33],[74,77,75,75,73],[19,18,21,26,28,32],[42,42,39,40,43,43],[54,49,52,50,44],[1,5,8,10,17,20,19],[69,72,71,70,66,64,62,62],[64,64,59,56,55,54,50],[31,26,28,27,26,26],[2,7,9,10,14,17,21],[77,80,83,85,88,89,86,88],[55,48,47,44,41,41,38,38],[46,46,52,54,56,59,60,59],[18,25,26,30,33,32],[89,86,82,81,76],[61,59,55,54,51,49,50],[23,27,29,29,29],[51,51,49,46,45,43,45,41],[79,83,84,87,87],[15,14,7,5,2,1,2],[14,15,14,16,17],[11,14,15,12,14,16,17,17],[30,30,33,35,38,39,46,46],[8,11,14,12,10,6],[42,39,39,37,37],[38,39,40,43,45,48,51,50],[36,42,43,46,47,48,46],[39,33,33,32,31,29],[60,60,57,58,56,55,56],[16,19,16,12,13],[35,35,37,38,41,40],[63,56,50,49,47,44],[34,32,30,30,28,25,19],[22,22,23,24,27,30,30,34],[96,92,92,91,88,85,86],[41,43,40,42,37],[51,49,45,42,39,39],[20,24,25,28,32],[65,69,73,74,76,78,80,78],[35,35,36,40,44],[32,39,41,48,52],[79,83,84,86,89,95],[56,60,61,64,61,60],[14,21,26,27,29,31,37],[58,58,51,48,45,42,39,39],[56,50,47,42,41,38,35,35],[91,87,84,78,77,75,75],[38,37,38,39,39,42,42],[64,65,66,67,68,71,74,76],[73,71,70,68,66,63,62,59],[18,21,23,24,27,29,31,32],[83,80,78,77,74,73],[28,30,31,34,37,40],[24,21,19,18,17,15],[64,61,58,56,55,52,49],[79,80,83,86,87,90,93],[51,52,55,56,59,60,62],[27,25,22,21,19,17],[75,78,80,81,83,85],[30,32,33,36,37,40,43],[49,48,47,45,43,40],[59,56,54,52,50,47,45,43],[32,31,29,28,27],[78,75,72,69,66,65,64],[47,45,42,41,39,36],[65,62,59,56,53,52],[84,85,86,88,89,91,93,95],[10,12,13,16,17,19,20],[94,92,90,87,84,83,82,81],[59,58,57,55,52,49,48],[2,3,4,6,9,12,13,14],[58,56,53,51,49,47,44,42],[33,36,38,40,42,44,46,47],[20,18,17,15,13,10,7,6],[88,86,83,80,79],[56,55,53,51,48,47],[69,72,74,75,78,79],[35,37,39,40,43,44,46],[25,22,20,18,15],[14,11,10,8,6,5],[50,52,55,57,59,60,61],[69,67,64,63,62,60],[90,89,86,85,82,81],[47,50,52,55,58,61],[17,20,22,23,25,28],[38,39,42,44,45,48],[44,45,48,49,50,51],[27,29,30,31,32,34,37,39],[50,53,55,56,57],[27,25,23,22,21,19,16,13],[77,80,83,86,89,91],[69,71,72,74,76,78],[16,17,18,19,21,23],[53,56,59,60,63],[68,70,73,76,79,81],[31,28,26,25,22,19],[45,44,41,40,39,38,36],[26,25,24,22,21,19],[76,77,79,81,84],[14,15,17,18,21],[31,33,34,35,37,40],[71,68,65,63,62],[78,79,80,82,85,88],[73,76,79,81,84,87],[96,95,94,93,91,90,89],[85,82,81,79,78,77,74],[80,77,76,73,72],[59,62,65,67,69,71],[18,19,22,24,27,29],[36,39,42,45,48,49],[17,18,20,23,24,26,29,31],[7,10,13,16,19],[47,46,43,41,38,35],[87,84,82,79,77,76,74,72],[76,75,72,71,70,69,68,65],[56,59,60,63,64],[25,28,31,34,35],[90,88,85,82,80,77,76],[51,50,47,46,43,40],[18,21,22,25,26],[12,11,9,8,6],[66,64,63,62,60,58,55],[63,62,61,58,57,56,55],[59,57,54,52,51,50,48],[51,49,48,45,42,39,38],[86,84,82,80,78,77,74,71],[39,40,42,44,45,47,49],[80,79,76,75,72,70],[88,85,84,82,79,78,75,73],[49,51,52,55,58,60,62,63],[71,70,69,67,66,64,62,60],[60,57,56,54,51,48,47,45],[79,77,74,72,70,68,65],[74,75,76,78,80,83,86,87],[12,11,9,6,4],[60,61,64,65,67,70,71,73],[14,11,9,7,6,5,2],[1,2,3,5,8,11,12],[73,74,77,78,80,82],[26,25,22,21,20,17],[76,75,74,71,70,69,68],[6,7,10,13,14,16,19],[69,66,65,64,61,58],[2,3,4,7,8,10,11,13],[94,91,89,87,86],[29,27,26,25,23,22],[12,15,17,19,22],[71,73,75,78,79],[68,69,71,73,76],[97,96,94,92,89,86],[57,58,60,61,63,65,68,69],[55,57,58,59,60,62],[78,81,83,84,85,87,88,89],[42,40,38,37,34,32,30],[64,61,59,56,53,50,48,46],[23,20,19,16,15,14,11],[76,78,81,83,86,87,88,90],[13,16,17,20,23],[4,5,6,7,10,11,13,14],[69,72,73,76,77,78,79,81],[77,75,73,71,69,67],[76,79,82,83,85,88,91],[65,68,69,70,72,73,75,78],[21,22,23,26,29,32,35,37],[68,66,65,64,63],[8,9,12,13,14,15],[59,58,55,52,50,47],[19,22,25,26,28,29],[41,38,35,32,30],[68,69,70,72,73,76,77,80],[27,26,25,23,20,18,17,14],[35,37,40,43,46,48,50,52],[13,12,9,6,3],[99,96,93,91,89],[20,19,18,16,14,13],[62,59,57,54,53,52],[46,49,51,54,55,57],[92,93,96,97,98],[88,86,84,81,79,76,74,72],[31,33,36,37,40,43,44],[74,71,69,67,64],[73,70,69,67,66,64,63],[71,70,67,66,64,61,60],[41,44,47,49,50],[98,96,94,91,88],[36,39,40,43,44,45,48],[47,48,51,52,55,58,60,61],[17,14,13,10,7],[73,74,76,79,81,83,86],[42,40,38,37,36,35],[51,52,53,55,57,60,63,65],[45,43,41,38,36,35,32,29],[9,10,13,14,16],[82,85,87,89,90,92,93,95],[34,36,38,39,42],[82,83,86,87,89,92],[14,13,12,11,10,9],[45,48,49,51,53,54,55,58],[59,58,56,53,52],[25,23,21,18,17,14,13,12],[35,36,38,41,44,46,48],[21,23,26,29,31,32,33],[94,92,90,87,84,82,81],[53,52,50,47,45,43],[69,71,74,77,80,81,83,86],[47,48,51,53,56,57,58],[11,14,16,17,19,21,24],[10,13,15,16,18],[70,67,65,63,62],[41,40,38,37,34,32,29,27],[19,18,15,14,13],[49,51,52,53,54],[43,44,45,46,48,50,51,52],[28,25,24,23,22,19,18],[87,89,90,93,95,97,99],[38,39,41,43,45,48,51],[20,19,16,15,14,12,10],[87,88,89,92,95,98],[14,17,18,20,23,26,28],[41,39,36,33,32,29,27,26],[10,8,7,5,2],[10,12,15,18,21],[78,80,81,82,84,86],[31,32,34,36,38],[41,44,46,47,48,51],[19,18,15,12,9,8,6,5],[79,78,75,74,73],[24,23,21,18,15,12],[74,73,71,68,67,66,63,61],[57,55,54,52,50,47],[88,85,84,82,80,79,77],[22,23,25,26,27,30],[54,55,57,58,60,61],[55,52,51,50,48,47,45],[26,24,23,22,21],[54,56,59,60,63,65,66,68],[12,9,8,5,3],[55,52,51,48,47,45,44,43],[78,75,74,72,69],[56,53,50,49,47],[86,84,81,78,77,76,74,71],[63,60,59,57,56,55],[1,3,6,7,9,10,12],[92,89,87,86,84,81,79,78],[26,23,20,18,16,13],[2,4,5,8,11,13],[53,56,58,61,63,66,67],[33,30,27,24,21,19],[80,83,85,87,90],[88,86,85,84,82,79]]

main :: IO ()
main = do
    input_list <- parseFile "aoc_2_1_input.txt"
    print (sum (map (`checkList` 0) input_list))

--- >>> main
