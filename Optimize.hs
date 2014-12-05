module Optimize(optimalPathToState,
                 SubPath,
                 State(..),
                 Cost) where

import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing, Ordering)
import Data.Maybe (isNothing, isJust)

import Types
import RateChord (Rating, perfectRating)

type Cost = Rating
type SubPath = ([State], Cost)

data State = InitialState
          | GoalState [(State, Rating)]
          | State Hand Rating [(State, Rating)]

-- | Comparison for 'Maybe' 'SubPath's. Nothing is greater than everything.
compareSubPaths :: Maybe SubPath -> Maybe SubPath -> Ordering
compareSubPaths Nothing _ = GT
compareSubPaths _ Nothing = LT
compareSubPaths (Just (_, lhsPathCost)) (Just (_, rhsPathCost)) = lhsPathCost `compare` rhsPathCost

-- | Comparison for Ancestors based on the cheapest path to the ancestor.
compareAncestors :: (State, Rating) -> (State, Rating) -> Ordering
compareAncestors lhs rhs = compareSubPaths lhsPath rhsPath
    where lhsPath = optimalPathToState (fst lhs)
          rhsPath = optimalPathToState (fst rhs)

-- | If there is a path from the 'InitialState' to the 'State' passed as the first argument, this will return the optimal path from the 'InitialState' to it.
optimalPathToState :: State -> Maybe SubPath
optimalPathToState InitialState = Just ([], perfectRating)
optimalPathToState state@(GoalState ancestors) = optimalPathToState' state perfectRating ancestors
optimalPathToState state@(State _ rating ancestors) = optimalPathToState' state rating ancestors

-- | Utility function to avoid duplication between 'GoalState' and 'State' handling.
optimalPathToState' :: State -> Rating -> [(State, Rating)] -> Maybe SubPath
optimalPathToState' state rating ancestors
    | null ancestors = Nothing
    | otherwise = 
        let bestAncestor = fst $ minimumBy compareAncestors ancestors
        in optimalPathToState bestAncestor >>= \(statesToAncestor, costToAncestor) ->
            Just (state:statesToAncestor, costToAncestor + rating)
