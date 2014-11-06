module Utils where

-- | Builds a list where every element from the input list appears in a pair with
-- every other element.
-- For example:
-- combinePairs [a, b, c, d] == [(a,b), (a,c), (a,d), (b,c), (b,d), (c,d)]
combinePairs :: [a] -> [(a, a)]
combinePairs = combinePairsWith (,)

combinePairsWith :: (a -> a -> b) -> [a] -> [b]
combinePairsWith _ [] = []
combinePairsWith f (x:xs) = map (f x) xs ++ combinePairsWith f xs

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (x, y) = (min x y, max x y)
