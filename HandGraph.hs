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
import Utils (allCombinationsWith, combineNeighbors)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.SP as SP
import Data.Graph.Inductive.Tree (Gr(..))
--import Data.Graph.Inductive.PatriciaTree (Gr(..))

-- | The current 'ChordTransitionRater's need no more than two adjacent hands, so a pair is sufficient. This has to be changed if higher-order rules are added (such as some counterpoint rules).
data Contents = Contents PositionInSong PositionInSong
    deriving(Show)

data PositionInSong = Start
                    | RatedHand Hand Rating
                    | End
    deriving (Show)

-- | A node contains two 'PositionInSong's
type Node = Graph.LNode Contents

-- | An edge between two 'Node's, with a 'Rating'.
type Edge = Graph.LEdge Rating

makeGraph :: [ChordSymbol] -> Gr Contents Rating
makeGraph chordSymbols =
    let ratedHands = map ratedHandsForChordSymbol chordSymbols :: [[PositionInSong]]
        withStartAndEnd = [Start] : ratedHands ++ [[End]]
        transitions = combineNeighbors (allCombinationsWith Contents) withStartAndEnd
        withIndices = addIndices transitions
        edges = makeEdges withIndices
        nodes = concat withIndices
    in Graph.mkGraph nodes edges

-- | All possible 'Hand's for a given 'ChordSymbol', each with its total 'Rating'.
ratedHandsForChordSymbol :: ChordSymbol -> [PositionInSong]
ratedHandsForChordSymbol chordSymbol =
    let hands = handsForChordSymbol chordSymbol
    in zipWith RatedHand hands (map totalRating hands)

addIndices :: [[Contents]] -> [[Node]]
addIndices = addIndices' 0

addIndices' startingIndex (currentLevel:rest) =
    let currentLength = length currentLevel
        indices = take currentLength [startingIndex..]
        nodes = zip indices currentLevel
    in nodes : addIndices' currentLength rest 
addIndices' _ [] = []

makeEdges :: [[Node]] -> [Edge]
makeEdges = concat . combineNeighbors (allCombinationsWith makeEdge)
    where makeEdge (lhsIndex, lhsContents) (rhsIndex, rhsContents) =
            (lhsIndex, rhsIndex, edgeCost lhsContents rhsContents)

edgeCost :: Contents -> Contents -> Rating
edgeCost _ (Contents Start _) = perfectRating
edgeCost _ (Contents _ Start) = error "Transitioning into Start!"
edgeCost _ (Contents End _)   = error "Transitioning out of End!"
edgeCost _ (Contents (RatedHand _ r) End) = r
edgeCost _ (Contents (RatedHand h r) (RatedHand h' _)) = r + totalTransitionRating (h, h')

