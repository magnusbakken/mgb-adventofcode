import Control.Monad (guard)
import qualified Data.List as L
import AdventOfCodeUtils (shortestLists)

inputTarget :: Int
inputTarget = 150
    
input :: [Int]
input = [50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40]

containerCombos :: Int -> [Int] -> [[Int]]
containerCombos target = filter ((== target) . sum) . L.subsequences

combinationCount :: Int -> [Int] -> Int
combinationCount target xs = length (containerCombos target xs)

minCombinationCount :: Int -> [Int] -> Int
minCombinationCount target xs = length (shortestLists (containerCombos target xs))

main :: IO ()
main = do
  print (combinationCount inputTarget input)
  print (minCombinationCount inputTarget input)
