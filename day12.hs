import Text.JSON
import Text.JSON.Types

decodeJSON :: String -> JSValue
decodeJSON s =
    case decode s of
      Error err -> error $ "Unable to parse: " ++ err
      Ok result -> result

sumJSONNumbers :: JSValue -> Int
sumJSONNumbers (JSRational _ r) = fromEnum r
sumJSONNumbers (JSArray array) = sum (map sumJSONNumbers array)
sumJSONNumbers (JSObject (JSONObject values)) = sum (map (sumJSONNumbers . snd) values)
sumJSONNumbers _ = 0

sumJSONNumbers2 :: JSValue -> Int
sumJSONNumbers2 (JSRational _ r) = fromEnum r
sumJSONNumbers2 (JSArray array) = sum (map sumJSONNumbers2 array)
sumJSONNumbers2 (JSObject (JSONObject values))
    | hasIgnorableValue (JSONObject values) = 0
    | otherwise = sum (map (sumJSONNumbers2 . snd) values)
sumJSONNumbers2 _ = 0

hasIgnorableValue :: JSObject JSValue -> Bool
hasIgnorableValue (JSONObject values) = any (isIgnorableValue . snd) values where
    isIgnorableValue (JSString s) = fromJSString s == "red"
    isIgnorableValue _ = False

readInput :: IO String
readInput = readFile "day12.txt"

main :: IO ()
main = do
  input <- readInput
  let json = decodeJSON input
  print (sumJSONNumbers json)
  print (sumJSONNumbers2 json)
