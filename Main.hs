module Main where

import qualified Data.Map as Map

import Types
import ConstrainChord
import Piano
import RateSubProgression
import GenerateChords
import ExampleChordProgressions
import HandProgressionGraph
import Utils
import LilypondOutput

-- | Get the 'ChordRating' for a given 'Hand', if it satisfied all 'ChordConstraint's.
evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just . totalRating . return $ hand
    | otherwise = Nothing

main :: IO ()
main = do
    let allOfMe_hands = optimalHandProgression allOfMe
    putStrLn . show . map showHand $ allOfMe_hands
    return ()
