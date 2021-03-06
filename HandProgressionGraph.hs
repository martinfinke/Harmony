{-|
Module      : HandProgressionGraph
Description : Generates a graph of all possible 'Hand's for a given 'ChordSymbol' progression.
-}
module HandProgressionGraph where

import Types
import RateSubProgression (totalRating)
import ExampleChordProgressions
import GenerateChords
import ConstrainChord (checkAllConstraints)
import Utils (slidingWindow, combineNeighbors2)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Graph.Inductive.Query.SP as SP
import qualified Data.Graph.Inductive.Graph as Graph
--import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.PatriciaTree (Gr(..))

type HandProgressionGraph = Gr SubProgression Rating
type Node = Graph.LNode SubProgression
type Edge = Graph.LEdge Rating

-- | The maximum number of 'Hand's examined by any 'SubProgressionRater'.
-- This is the maximum length of the '[Hand]' list passed to each 'SubProgressionRater' function.
-- At the beginning of the song, the list is shorter. The list lengths are [1..subProgressionLength].
subProgressionLength :: Int
subProgressionLength = 2

makeGraph :: [ChordSymbol] -> HandProgressionGraph
makeGraph chordSymbols =
    let ratedHands = map ratedHandsForChordSymbol chordSymbols :: [[RatedHand]]
        transitions = makeSubProgressions ratedHands
        withStartAndEnd = [Start] : transitions ++ [[End]]
        withIndices = addIndices 0 withStartAndEnd
        edges = makeEdges withIndices
        nodes = concat withIndices
    in Graph.mkGraph nodes edges

-- | All possible 'Hand's for a given 'ChordSymbol', each with its total 'Rating'.
ratedHandsForChordSymbol :: ChordSymbol -> [RatedHand]
ratedHandsForChordSymbol chordSymbol =
    let hands = filter checkAllConstraints $ handsForChordSymbol chordSymbol
        ratings = map (totalRating . return) hands
    in zipWith RatedHand hands ratings

addIndices :: Int -> [[a]] -> [[Graph.LNode a]]
addIndices startingIndex (currentLevel:rest) =
    let currentLength = length currentLevel
        indices = take currentLength [startingIndex..]
        nodes = zip indices currentLevel
    in nodes : addIndices (startingIndex + currentLength) rest 
addIndices _ [] = []

makeEdges :: [[Node]] -> [Edge]
makeEdges = concat . combineNeighbors2 makeNeighborEdges

makeNeighborEdges :: [Node] -> [Node] -> [Edge]
makeNeighborEdges (firstLeft:restLeft) rhsCol =
    combineOne firstLeft rhsCol ++ makeNeighborEdges restLeft rhsCol
    where makeEdge (lhsIndex, lhsContents) (rhsIndex, rhsContents) = do
            if shouldMakeNeighborEdge lhsContents rhsContents
                then Just (lhsIndex, rhsIndex, edgeCost lhsContents rhsContents)
                else Nothing
          combineOne lhsElem rhsCol = catMaybes $ map (makeEdge lhsElem) rhsCol
makeNeighborEdges [] _ = []

-- | Determines whether an edge should be created to link two 'SubProgression' nodes.
-- From the 'Start' and to the 'End' there should be edges to all neighboring nodes.
-- For neighboring 'SubProgression' nodes, it depends on whether both sides' relevant hands are equal. See 'relevantLhsHand' and 'relevantRhsHand'.
shouldMakeNeighborEdge :: SubProgression -> SubProgression -> Bool
shouldMakeNeighborEdge Start _ = True
shouldMakeNeighborEdge _ End = True
shouldMakeNeighborEdge (SubProgression lhsHands) (SubProgression rhsHands) = relevantLhs == relevantRhs
    where u = length lhsHands
          v = length rhsHands
          relevantLhs = map hand $ drop (u-v+1) lhsHands
          relevantRhs = map hand $ take (v-1) rhsHands

makeSubProgressions :: [[RatedHand]] -> [[SubProgression]]
makeSubProgressions ratedHands = slidingWindow subProgressionLength SubProgression ratedHands

edgeCost :: SubProgression -> SubProgression -> Rating
edgeCost End _ = error "Transitioning out of End!"
edgeCost _ Start = error "Transitioning into Start!"
edgeCost _ End = perfectRating
edgeCost _ (SubProgression hands) =
    totalRating $ map hand $ reverse hands

optimalHandProgression :: [ChordSymbol] -> [Hand]
optimalHandProgression = optimalHandProgressionForGraph . makeGraph

minimumCost :: [ChordSymbol] -> Rating
minimumCost = snd . bestIndexPath . makeGraph

optimalHandProgressionForGraph :: HandProgressionGraph -> [Hand]
optimalHandProgressionForGraph graph =
    let (indexPath, _) = bestIndexPath graph
        subProgressionNodes = map (Graph.lab graph) indexPath :: [Maybe SubProgression]
    in subProgressionsToHands subProgressionNodes

bestIndexPath :: HandProgressionGraph -> (Graph.Path, Rating)
bestIndexPath graph =
    let (start, end) = Graph.nodeRange graph
    in (SP.sp start end graph, SP.spLength start end graph)

-- Take only the first RatedHand, except for the last SubProgression. For that one, take the tail.
subProgressionsToHands :: [Maybe SubProgression] -> [Hand]
subProgressionsToHands list
    | all isJust list = subProgressionsToHands' $ catMaybes list
    | otherwise = error "Gap in output chord progression."

subProgressionsToHands' :: [SubProgression] -> [Hand]
subProgressionsToHands' (Start:rest) = subProgressionsToHands' rest
subProgressionsToHands' (End:[]) = []
subProgressionsToHands' ((SubProgression firstHands):others) = hand (last firstHands) : subProgressionsToHands' others
