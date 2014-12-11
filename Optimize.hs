{-|
Module      : Optimize
Description : Find the optimal 'Hand' progression, given a 'State' graph.

The current implementation is very naive and completely uninformed, making it simple and very slow.
-}
module Optimize where

import Data.List (minimumBy)
import Data.Maybe (catMaybes)

import Types (Hand)
import RateChord (Rating, perfectRating)
import GenerateChords(State(..))

-- | Edge cost
type Cost = Rating

-- | A path through the 'State' graph, with a total 'Cost'.
type Path = ([State], Cost)

-- | Comparison for 'Maybe' 'Path's. 'Data.Maybe.Nothing' is greater than everything.
comparePaths :: Maybe Path -> Maybe Path -> Ordering
comparePaths Nothing _ = GT
comparePaths _ Nothing = LT
comparePaths (Just (_, lhsPathCost)) (Just (_, rhsPathCost)) = lhsPathCost `compare` rhsPathCost

-- | Comparison for Ancestors based on the cheapest path to the ancestor.
compareAncestors :: (State, Rating) -> (State, Rating) -> Ordering
compareAncestors lhs rhs = comparePaths lhsPath rhsPath
    where lhsPath = optimalPathToState (fst lhs)
          rhsPath = optimalPathToState (fst rhs)

-- | If there is a path from the 'InitialState' to the 'State' passed as the first argument, this will return the optimal path from the 'InitialState' to it.
optimalPathToState :: State -> Maybe Path
optimalPathToState InitialState = Just ([], perfectRating)
optimalPathToState state@(GoalState ancestors) = optimalPathToState' state perfectRating ancestors
optimalPathToState state@(State _ rating ancestors) = optimalPathToState' state rating ancestors

-- | Utility function to avoid duplication between 'GoalState' and 'State' handling.
optimalPathToState' :: State -> Rating -> [(State, Rating)] -> Maybe Path
optimalPathToState' state rating ancestors
    | null ancestors = Nothing
    | otherwise = 
        let bestAncestor = fst $ minimumBy compareAncestors ancestors
        in optimalPathToState bestAncestor >>= \(statesToAncestor, costToAncestor) ->
            Just (state:statesToAncestor, costToAncestor + rating)

-- | Extracts the 'Hand' progression from a 'Path', removing the 'InitialState' and 'GoalState'.
pathToHandProgression :: Path -> [Hand]
pathToHandProgression ([], _) = []
pathToHandProgression (states, _) = reverse . catMaybes $ map stateToHand states
    where stateToHand state = case state of
            (State hand _ _) -> Just hand
            _ -> Nothing
