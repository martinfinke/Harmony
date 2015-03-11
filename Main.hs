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
import qualified Data.Map as Map (lookup)

import System.Environment (getArgs)

-- | Get the 'ChordRating' for a given 'Hand', if it satisfied all 'ChordConstraint's.
evalulateHand :: Hand -> Maybe Rating
evalulateHand hand
    | checkAllConstraints hand = Just . totalRating . return $ hand
    | otherwise = Nothing

main :: IO ()
main = do
    args <- getArgs
    let exampleName = case args of
            first:_ -> first
            [] -> error $ "Please provide the name of an example. Possible choices: " ++ (show $ map fst $ Map.toList examplesByName)

    let chordSymbols = case Map.lookup exampleName examplesByName of
            Just cSyms -> cSyms
            Nothing -> error "Invalid example name."
    let hands = optimalHandProgression chordSymbols
    putStrLn . show . map showHand $ hands
