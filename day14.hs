import Control.Monad
import Data.List (foldl')
import AdventOfCodeUtils (sortWith)

type Speed = Int
type Time = Int
type Position = Int
type Points = Int

data Reindeer = Reindeer {
      name :: String,
      speed :: Speed,
      flyTime :: Time,
      restTime :: Time
    }

data ReindeerPos = ReindeerPos {
      reindeer :: Reindeer,
      position :: Position,
      points :: Points,
      isResting :: Bool,
      timeSpent :: Time
    }

instance Show Reindeer where
    show r = "[" ++ name r ++ ": (Speed: " ++ show (speed r) ++ " km/s, FlyTime: " ++ show (flyTime r) ++ " s, RestTime: " ++ show (restTime r) ++ " s)]"

input :: [Reindeer]
input = [Reindeer "Vixen" 19 7 124,
         Reindeer "Rudolph" 3 15 28,
         Reindeer "Donner" 19 9 164,
         Reindeer "Blitzen" 19 9 158,
         Reindeer "Comet" 13 7 82,
         Reindeer "Cupid" 25 6 145,
         Reindeer "Dasher" 14 3 38,
         Reindeer "Dancer" 3 16 37,
         Reindeer "Prancer" 25 6 143]

updateReindeer :: ReindeerPos -> ReindeerPos
updateReindeer (ReindeerPos reindeer position pts isResting timeSpent)
    | comeToRest = ReindeerPos reindeer updatedPosition pts True 0
    | startBackUp = ReindeerPos reindeer updatedPosition pts False 0
    | otherwise = ReindeerPos reindeer updatedPosition pts isResting newTimeSpent
    where
      newTimeSpent = succ timeSpent
      comeToRest = not isResting && newTimeSpent == flyTime reindeer
      startBackUp = isResting && newTimeSpent == restTime reindeer
      updatedPosition = if isResting then position else position + speed reindeer

addPointsIfLeading :: Position -> ReindeerPos -> ReindeerPos
addPointsIfLeading leadingPosition (ReindeerPos reindeer position pts isResting timeSpent)
    | position == leadingPosition = ReindeerPos reindeer position (succ pts) isResting timeSpent
    | otherwise = ReindeerPos reindeer position pts isResting timeSpent

initialReindeerPositions :: [Reindeer] -> [ReindeerPos]
initialReindeerPositions = map (\r -> ReindeerPos r 0 0 False 0)

simulateRace :: Time -> [Reindeer] -> [ReindeerPos]
simulateRace totalTime rs =
    foldl' (\r _ -> map updateReindeer r) (initialReindeerPositions rs) [1..totalTime]

simulateRace2 :: Time -> [Reindeer] -> [ReindeerPos]
simulateRace2 totalTime rs =
    foldl' update (initialReindeerPositions rs) [1..totalTime] where
        update rs _ =
          let allUpdated = map updateReindeer rs
              leadingPosition = maximum (map position allUpdated) in
          addPointsIfLeading leadingPosition <$> allUpdated

winningReindeer :: [ReindeerPos] -> ReindeerPos
winningReindeer = last . sortWith position

winningReindeer2 :: [ReindeerPos] -> ReindeerPos
winningReindeer2 = last . sortWith points

winningReindeerPosition :: Time -> [Reindeer] -> Position
winningReindeerPosition totalTime rs = position (winningReindeer (simulateRace totalTime rs))

winningReindeerPosition2 :: Time -> [Reindeer] -> Position
winningReindeerPosition2 totalTime rs = points (winningReindeer2 (simulateRace2 totalTime rs))

main :: IO ()
main = do
  print (winningReindeerPosition 2503 input)
  print (winningReindeerPosition2 2503 input)
