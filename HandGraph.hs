{-|
Module      : HandGraph
Description : Generates a graph of all possible 'Hand's for a given 'ChordSymbol' progression.
-}
module HandGraph where

import Types
import RateChord (Rating, totalRating, perfectRating)
import RateChordTransition (totalTransitionRating)
import ExampleChordProgressions
import GenerateChords
import ConstrainChord (checkAllConstraints)
import Utils (allCombinationsWith, allCombinationsVariableLength, combineNeighborsVariableLength, combineNeighbors2)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.SP as SP
--import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Maybe (catMaybes, isJust)

-- | The current 'ChordTransitionRater's need no more than two adjacent hands, so a pair is sufficient. This has to be changed if higher-order rules are added (such as some counterpoint rules).
data Contents = Start
              | HandGroup [RatedHand]
              | End
    deriving(Show)

handGroupLength = 2

getRatedHand :: ([RatedHand] -> RatedHand) -> [RatedHand] -> RatedHand
getRatedHand selector hands
    | null hands = error "Empty hands group."
    | otherwise = selector hands

relevantLhsHand, relevantRhsHand :: [RatedHand] -> RatedHand
relevantLhsHand = getRatedHand last
relevantRhsHand = getRatedHand (last . init)

data RatedHand = RatedHand {hand :: Hand, rating :: Rating}
    deriving(Show)

makeGraph :: [ChordSymbol] -> Gr Contents Rating
makeGraph chordSymbols =
    let ratedHands = map ratedHandsForChordSymbol chordSymbols :: [[RatedHand]]
        transitions = makeTransitions ratedHands
        withStartAndEnd = [Start] : transitions ++ [[End]]
        withIndices = addIndices 0 withStartAndEnd
        edges = makeEdges withIndices
        nodes = concat withIndices
    in Graph.mkGraph nodes edges

-- | All possible 'Hand's for a given 'ChordSymbol', each with its total 'Rating'.
ratedHandsForChordSymbol :: ChordSymbol -> [RatedHand]
ratedHandsForChordSymbol chordSymbol =
    let hands = filter checkAllConstraints $ handsForChordSymbol chordSymbol
        ratings = map totalRating hands
    in zipWith RatedHand hands ratings

addIndices :: Int -> [[a]] -> [[Graph.LNode a]]
addIndices startingIndex (currentLevel:rest) =
    let currentLength = length currentLevel
        indices = take currentLength [startingIndex..]
        nodes = zip indices currentLevel
    in nodes : addIndices (startingIndex+currentLength) rest 
addIndices _ [] = []

makeEdges :: [[Graph.LNode Contents]] -> [Graph.LEdge Rating]
makeEdges = concat . combineNeighbors2 makeNeighborEdges

makeNeighborEdges :: [Graph.LNode Contents] -> [Graph.LNode Contents] -> [Graph.LEdge Rating]
makeNeighborEdges (firstLeft:restLeft) rhsCol =
    combineOne firstLeft rhsCol ++ makeNeighborEdges restLeft rhsCol
    where makeEdge (lhsIndex, lhsContents) (rhsIndex, rhsContents) = do
            if shouldMakeNeighborEdge lhsContents rhsContents
                then Just (lhsIndex, rhsIndex, edgeCost lhsContents rhsContents)
                else Nothing
          combineOne lhsElem rhsCol = catMaybes $ map (makeEdge lhsElem) rhsCol
makeNeighborEdges [] _ = []


-- | Determines whether an edge should be created to link two 'Contents' nodes.
-- From the 'Start' and to the 'End' there should be edges to all neighboring nodes.
-- For neighboring 'HandGroup' nodes, it depends on whether both sides' relevant hands are equal. See 'relevantLhsHand' and 'relevantRhsHand'.
shouldMakeNeighborEdge :: Contents -> Contents -> Bool
shouldMakeNeighborEdge Start _ = True
shouldMakeNeighborEdge _ End = True
shouldMakeNeighborEdge (HandGroup lhsHands) (HandGroup rhsHands) =
    (hand . relevantLhsHand) lhsHands == (hand . relevantRhsHand) rhsHands

makeTransitions :: [[RatedHand]] -> [[Contents]]
makeTransitions ratedHands = combineNeighborsVariableLength handGroupLength (allCombinationsVariableLength handGroupLength HandGroup) ratedHands

edgeCost :: Contents -> Contents -> Rating
edgeCost End _ = error "Transitioning out of End!"
edgeCost _ Start = error "Transitioning into Start!"
edgeCost Start (HandGroup (hand:_)) = rating hand
edgeCost _ End = perfectRating
edgeCost (HandGroup _) (HandGroup hands) =
    let reversedOrder@(lastHand:_) = reverse hands
        handRating = rating lastHand
        transitionRating = totalTransitionRating $ map hand reversedOrder
    in handRating + transitionRating

bestHandProgression :: [ChordSymbol] -> [Hand]
bestHandProgression = bestHandProgressionForGraph . makeGraph

bestHandProgressionForGraph :: Gr Contents Rating -> [Hand]
bestHandProgressionForGraph graph =
    let indexPath = bestIndexPath graph
        contentNodes = map (Graph.lab graph) indexPath :: [Maybe Contents]
    in contentsToHands contentNodes

bestIndexPath :: Gr Contents Rating -> Graph.Path
bestIndexPath graph =
    let (start, end) = Graph.nodeRange graph
    in SP.sp start end graph


-- Take only the first RatedHand, except for the last HandGroup. For that one, take the tail.
contentsToHands :: [Maybe Contents] -> [Hand]
contentsToHands list
    | null list = []
    | all isJust list = contentsToHands' $ catMaybes list
    | otherwise = error "Gap in output chord progression."

contentsToHands' :: [Contents] -> [Hand]
contentsToHands' (Start:rest) = contentsToHands' rest
contentsToHands' ((HandGroup hands):End:[]) = map hand hands
contentsToHands' ((HandGroup firstHands):rest) = hand (head firstHands) : contentsToHands' rest
