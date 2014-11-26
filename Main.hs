module Main where

import qualified Data.Map as Map

import Types
import ConstrainChord
import Piano
import RateChord
import RateChordTransition
import GenerateChords
import ExampleChordProgressions
import Tests hiding (main)

evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just $ totalRating hand
    | otherwise = Nothing

main :: IO ()
main = return ()
