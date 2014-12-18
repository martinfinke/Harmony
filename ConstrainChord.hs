{-|
Module      : ConstrainChord
Description : Definitions of 'ChordConstraint's such as maximum finger spreading.

If a chord doesn't satisfy all constraints, it won't be considered.
-}
module ConstrainChord where

import Types
import Piano
import Utils (combinePairsWith, sortPair)
import qualified Data.Map as Map
import Data.Array (inRange)

-- | A function that either accepts or rejects a 'Hand'.
type ChordConstraint = Hand -> Bool

-- | All 'ChordConstraint's in this module.
allChordConstraints :: [ChordConstraint]
allChordConstraints = [constrain_fingerSpreading,
                       constrain_insidePitchRange defaultAcceptedPitchRange]

-- | Checks whether a 'Hand' satisfies 'allChordConstraints'.
checkAllConstraints :: Hand -> Bool
checkAllConstraints hand = all ($ hand) allChordConstraints

-- | Ensures that no two 'Finger's in a hand exceed their allowed spread on a piano.
constrain_fingerSpreading :: ChordConstraint
constrain_fingerSpreading hand =
    all isAllowedDistance distances
    where combineFingerMappings (f, p) (f', p') = (sortPair (f, f'), distance (p, p'))
          distances = combinePairsWith combineFingerMappings $ Map.toList hand

-- | Ensures that all played notes are inside a given 'PitchRange'.
constrain_insidePitchRange :: PitchRange -> ChordConstraint
constrain_insidePitchRange (lowest, highest) =
    all (inRange (lowest, highest)) . map snd . Map.toList

-- | All generated 'Chord's should be inside this range.
defaultAcceptedPitchRange :: PitchRange
defaultAcceptedPitchRange = (toPitch C 3, toPitch C 6)
