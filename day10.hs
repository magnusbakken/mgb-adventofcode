import qualified Data.List as L

input :: String
input = "1321131112"
    
runLengthEncode :: String -> String
runLengthEncode s = L.group s >>= \s' -> show (length s') ++ [head s']

iterateRunLengthN :: Int -> String -> String
iterateRunLengthN n s = iterate runLengthEncode s !! n

main :: IO ()
main = do
  print $ length $ iterateRunLengthN 40 input
  print $ length $ iterateRunLengthN 50 input
