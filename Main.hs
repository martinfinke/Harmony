module Main where

import qualified Data.Map as Map

import Types
import ConstrainChord
import Piano
import RateChord
import RateChordTransition
import GenerateChords
import ExampleChordProgressions
import HandProgressionGraph
import Utils

-- | Get the 'ChordRating' for a given 'Hand', if it satisfied all 'ChordConstraint's.
evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just $ totalRating hand
    | otherwise = Nothing

-- | Bird's-eye view: Get the optimal 'Hand' progression for a 'ChordSymbol' progression.
optimalHandProgression :: [ChordSymbol] -> [Hand]
optimalHandProgression = bestHandProgression

main :: IO ()
main = return ()
