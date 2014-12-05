module Main where

import qualified Data.Map as Map

import Types
import ConstrainChord
import Piano
import RateChord
import RateChordTransition
import GenerateChords
import ExampleChordProgressions
import Optimize
import Tests hiding (main)

evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just $ totalRating hand
    | otherwise = Nothing


optimalHandProgression :: [ChordSymbol] -> [Hand]
optimalHandProgression chordSymbols = case optimalPathToState $ generateProgressions chordSymbols of
    Nothing -> error "Chord Progression not feasible."
    Just path -> pathToHandProgression path


main :: IO ()
main = return ()
