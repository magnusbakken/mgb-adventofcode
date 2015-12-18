import qualified Data.Array as A
import System.IO (readFile)

type Coords = (Int, Int)
type Lights = A.Array Coords Bool

isLightOn :: Lights -> Coords -> Bool
isLightOn = (A.!)

neighbors :: (Coords, Coords) -> Coords -> [Coords]
neighbors ((minX, minY), (maxX, maxY)) (x, y) = above ++ left ++ below ++ right where
    above
        | atTop = []
        | atLeft = [c2, c3]
        | atRight = [c1, c2]
        | otherwise = [c1, c2, c3]
    left = if atLeft then [] else [c4]
    below
        | atBottom = []
        | atLeft = [c7, c8]
        | atRight = [c6, c7]
        | otherwise = [c6, c7, c8]
    right = if atRight then [] else [c5]
    atTop = y == minY
    atBottom = y == maxY
    atLeft = x == minX
    atRight = x == maxX
    c1 = (pred x, pred y)
    c2 = (x, pred y)
    c3 = (succ x, pred y)
    c4 = (pred x, y)
    c5 = (succ x, y)
    c6 = (pred x, succ y)
    c7 = (x, succ y)
    c8 = (succ x, succ y)

corners :: (Coords, Coords) -> [Coords]
corners ((minX, minY), (maxX, maxY)) =
    [(minX, minY), (minX, maxY), (maxX, minY), (maxX, maxY)]

isCorner :: (Coords, Coords) -> Coords -> Bool
isCorner bounds coords = coords `elem` corners bounds
      
countLiveNeighbors :: Lights -> Coords -> Int
countLiveNeighbors lights coords =
    length (filter (isLightOn lights) (neighbors (A.bounds lights) coords))

shouldLightBeOn :: Lights -> Coords -> Bool -> Bool
shouldLightBeOn lights coords isOnNow =
    case countLiveNeighbors lights coords of
      3 -> True
      2 -> isOnNow
      _ -> False

shouldLightBeOn2 :: Lights -> Coords -> Bool -> Bool
shouldLightBeOn2 lights coords isOnNow
    | isCorner (A.bounds lights) coords = True
    | otherwise = shouldLightBeOn lights coords isOnNow

runGeneration :: (Lights -> Coords -> Bool -> Bool) -> Lights -> Lights
runGeneration f lights = A.array (A.bounds lights) (fmap alter (A.assocs lights)) where
    alter (coords, isOnNow) = (coords, f lights coords isOnNow)

simulate :: (Lights -> Coords -> Bool -> Bool) -> Int -> Lights -> Lights
simulate f n lights = iterate (runGeneration f) lights !! n

countActive :: Lights -> Int
countActive = length . filter id . A.elems

readInputString :: String -> Lights
readInputString s =
    A.array ((0,0), (99,99)) $ do
      (y, line) <- zip [0..] (lines s)
      (x, c) <- zip [0..] line
      return ((x,y), isOn c)
        where
          isOn '#' = True
          isOn '.' = False

readInput :: IO Lights
readInput = fmap readInputString (readFile "day19.txt")

turnOnCorners :: Lights -> Lights
turnOnCorners lights = (A.//) lights cornerUpdates where
    cornerUpdates = map (\x -> (x, True)) (corners (A.bounds lights))

main :: IO ()
main = do
  input <- readInput
  print (countActive (simulate shouldLightBeOn 100 input))
  let withCornersOn = turnOnCorners input
  print (countActive (simulate shouldLightBeOn2 100 withCornersOn))
