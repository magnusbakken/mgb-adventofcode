module AdventOfCodeUtils where

import Control.Applicative
import Data.Function (on)
import qualified Data.List as L
                
smallestTwo :: Ord a => a -> a -> a -> (a, a)
smallestTwo a b c
    | a >= b = (b, min a c)
    | otherwise = (a, min b c)

tupleToList2 :: (a, a) -> [a]
tupleToList2 (a, b) = [a, b]

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
         continue = target == n || go (succ n) xs

hasDouble :: Eq a => [a] -> Bool
hasDouble s = any (uncurry (==)) (zip s (tail s))

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = L.sortBy (compare `on` f)

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx n x l = take n l ++ [x] ++ drop (succ n) l

annotate :: (a -> b) -> [a] -> [(a, b)]
annotate f = map (\x -> (x, f x))

listsByLength :: Foldable t => [t a] -> [t a]
listsByLength = sortWith length

shortestLists :: [[a]] -> [[a]]
shortestLists xs = case listsByLength xs of
                     [] -> []
                     (x:xs) -> x : takeWhile (\y -> length y == length x) xs

neighbors :: (Enum a, Eq a) => (a, a) -> [(a, a)]
neighbors (x, y) = filter (/= (x, y)) combos where
    combos = [pred, id, succ] >>= \f1 -> [pred, id, succ] >>= \f2 -> return (f1 x, f2 y)

neighborsWithin :: (Ord a, Enum a, Eq a) => ((a, a), (a, a)) -> (a, a) -> [(a, a)]
neighborsWithin ((minX, minY), (maxX, maxY)) coords = filter within (neighbors coords) where
    within (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
