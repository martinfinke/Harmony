module Main where

import qualified Data.Map as Map

import Types
import ConstrainChord
import RateChord

evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just $ totalRating hand
    | otherwise = Nothing

main :: IO ()
main = return ()
