{-|
Module      : Utils
Description : Utility functions which may be replaced by smart Haskell builtins, if possible.
-}
module Utils where

-- | Builds a list where every element from the input list appears in a pair with
-- every other element.
-- For example:
--
-- >>> combinePairs [a, b, c, d]
-- [(a,b), (a,c), (a,d), (b,c), (b,d), (c,d)]
combinePairs :: [a] -> [(a, a)]
combinePairs = combinePairsWith (,)

-- | General form of combinePairs. Takes a custom combination function.
-- For example:
--
-- >>> combinePairsWith (+) [1, 10, 100, 1000]
-- [11, 101, 1001, 110, 1010, 1100]
combinePairsWith :: (a -> a -> b) -> [a] -> [b]
combinePairsWith _ [] = []
combinePairsWith f (x:xs) = map (f x) xs ++ combinePairsWith f xs

-- | Sorts the two elements of a pair in ascending order.
sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y) = (min x y, max x y)

allTuples :: [a] -> [b] -> [(a, b)]
allTuples = allCombinationsWith (,)

allCombinationsWith :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombinationsWith f (x:xs) ys = map (f x) ys ++ allCombinationsWith f xs ys
allCombinationsWith _ [] _ = []

-- | Combines all neighbors of a list into pairs.
-- The first value in the list only appears once ('fst' in the first tuple), the last value appears only once ('snd' in the last tuple). All other values appear twice, once as 'fst', and once as 'snd'.
-- If the list has a 'Prelude.length' < 2, the result is the empty list. Otherwise, the length of the result list is the length of the input list - 1.
-- >>> combineNeighborsPairwise [a, b, c, d]
-- [(a,b), (b,c), (c,d)]
combineNeighborsPairwise :: [a] -> [(a, a)]
combineNeighborsPairwise = combineNeighbors (,)

-- | Generic version of 'combineNeighborsPairwise'.
combineNeighbors :: (d -> d -> e) -> [d] -> [e]
combineNeighbors f (x:x':xs) = f x x': combineNeighbors f (x':xs)
combineNeighbors _ (_:[]) = []
combineNeighbors _ [] = []
