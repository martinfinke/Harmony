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

allCombinationsVariableLength :: Int -> ([a] -> b) -> [[a]] -> [b]
allCombinationsVariableLength len f matrix =
    map f $ stepwiseCombine matrix
    where stepwiseCombine matrix
            | null matrix = []
            | length matrix <= len = allPathsThroughColumns matrix
            | otherwise = allPathsThroughColumns (take len matrix) ++ stepwiseCombine (tail matrix)
          allPathsThroughColumns matrix = case matrix of
            --(lastCol:[]) -> map (:[]) lastCol
            [] -> [[]]
            (firstCol:otherCols) ->
                let rest = allPathsThroughColumns otherCols
                in allCombinationsWith (:) firstCol rest

-- | Combines all neighbors of a list into pairs.
-- The first value in the list only appears once ('fst' in the first tuple), the last value appears only once ('snd' in the last tuple). All other values appear twice, once as 'fst', and once as 'snd'.
-- If the list has a 'Prelude.length' < 2, the result is the empty list. Otherwise, the length of the result list is the length of the input list - 1.
-- >>> combineNeighborsPairwise [a, b, c, d]
-- [(a,b), (b,c), (c,d)]
combineNeighborsPairwise :: [a] -> [(a, a)]
combineNeighborsPairwise = combineNeighbors2 (,)

-- | Generic version of 'combineNeighborsPairwise'.
combineNeighbors2 :: (a -> a -> b) -> [a] -> [b]
combineNeighbors2 f (x:x':xs) = f x x': combineNeighbors2 f (x':xs)
combineNeighbors2 _ (_:[]) = []
combineNeighbors2 _ [] = []

combineNeighborsVariableLength :: Int -- ^ The maximum length of a neighbor group. If the input list is shorter than this value, the group length will be the length of the input list.
                 -> ([a] -> b) -- ^ A function combining neighbors into a group
                 -> [a] -- ^ The list to be grouped into neighbors
                 -> [b]
combineNeighborsVariableLength maxGroupLength f list
    | maxGroupLength == 0 = error "Trying to group into groups of length 0."
    | null list = []
    | length list < maxGroupLength = [f list]
    | otherwise = combineNeighbors' list
    where combineNeighbors' ls
            | length ls < maxGroupLength = []
            | otherwise = let toGroup = take maxGroupLength ls
                              remainder = tail ls
                          in f toGroup : combineNeighbors' remainder
