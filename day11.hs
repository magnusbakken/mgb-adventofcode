input :: String
input = "cqjxjnds"

hasThreeStraight :: String -> Bool
hasThreeStraight s = any areConsecutive triplets where
    triplets = zip3 s (drop 1 s) (drop 2 s)
    areConsecutive (a, b, c) = a == pred b && b == pred c

hasInvalidLetters :: String -> Bool
hasInvalidLetters = any (`elem` invalidLetters) where
    invalidLetters = "iol"

hasNPairs :: Int -> String -> Bool
hasNPairs n s = go s '\0' False 0 where
    go [] _ _ _ = False
    go (c:cs) prev hadPair nPairs
       | isPair && hasNPairs = True
       | otherwise = go cs c isPair newNPairs
       where
         isPair = c == prev && not hadPair
         hasNPairs = nPairs >= n-1
         newNPairs = if isPair then succ nPairs else nPairs

isPasswordValid :: String -> Bool
isPasswordValid s = hasThreeStraight s && not (hasInvalidLetters s) && hasNPairs 2 s

incrementPassword :: String -> String
incrementPassword s = reverse (incrementString (reverse s)) where
    incrementString [] = error "Empty string"
    incrementString "z" = error "Final possible password reached"
    incrementString [c] = [succ c]
    incrementString ('z':cs) = 'a' : incrementString cs
    incrementString (c:cs) = succ c : cs

nextConceivable :: String -> String
nextConceivable [] = []
nextConceivable (c:cs)
    | c `elem` "iol" = succ c : replicate (length cs) 'a'
    | otherwise = c : nextConceivable cs

nextPassword :: String -> String
nextPassword s = until isPasswordValid (incrementPassword . nextConceivable) (incrementPassword (nextConceivable s))

main :: IO ()
main = do
  let firstPassword = nextPassword input
  putStrLn firstPassword
  putStrLn (nextPassword firstPassword)
