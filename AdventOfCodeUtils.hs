module AdventOfCodeUtils where

import qualified Data.List as L
                
smallestTwo :: Ord a => a -> a -> a -> (a, a)
smallestTwo a b c
    | a >= b = (b, min a c)
    | otherwise = (a, min b c)

listToTuple2 :: [a] -> (a, a)
listToTuple2 ([a, b]) = (a, b)
listToTuple2 _ = error "List does not contain exactly two items"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 ([a, b, c]) = (a, b, c)
listToTuple3 _ = error "List does not contain exactly three items"

hasPrefixN :: Eq a => Int -> a -> [a] -> Bool
hasPrefixN n c s = replicate n c == take n s

trueOfN :: Int -> (a -> Bool) -> [a] -> Bool
trueOfN target f = go 0 where
    go n [] = target == n
    go n (x:xs)
       | f x = continue
       | otherwise = go n xs
       where
         continue = if target == n then True else go (succ n) xs

hasDouble :: Eq a => [a] -> Bool
hasDouble s = any (uncurry (==)) (zip s (drop 1 s))

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx n x l = take n l ++ [x] ++ drop (succ n) l

annotate :: (a -> b) -> [a] -> [(a, b)]
annotate f = map (\x -> (x, f x))

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 f xs = map fst (filter (uncurry f) (zip xs (tail xs)))

listsByLength :: [[a]] -> [[a]]
listsByLength = map fst . L.sortBy cmp . annotate length where
    cmp (_, t1) (_, t2) = t1 `compare` t2

shortestLists :: [[a]] -> [[a]]
shortestLists xs = case listsByLength xs of
                     [] -> []
                     (x:xs) -> x : takeWhile (\y -> length y == length x) xs
