module ConstrainChord where

import Types
import Piano (defaultMaxFingerSpread)
import Utils (combinePairsWith, sortTuple)
import qualified Data.Map as Map

-- | A function that either accepts or rejects a 'Hand'.
type ChordConstraint = Hand -> Bool

allChordConstraints :: [ChordConstraint]
allChordConstraints = [constrain_fingerSpreading,
                       constrain_insidePitchRange defaultAcceptedPitchRange]

-- | Checks whether a 'Hand' follows all constraints.
checkAllConstraints :: Hand -> Bool
checkAllConstraints hand = all ($ hand) allChordConstraints

-- | Ensures that no two 'Finger's in a hand exceed their allowed spread on a piano.
constrain_fingerSpreading :: ChordConstraint
constrain_fingerSpreading hand =
    all checkInterval intervals
    where combineFingerMappings (f, p) (f', p') = (sortTuple (f, f'), absInterval p p')
          intervals = combinePairsWith combineFingerMappings $ Map.toList hand
          checkInterval (fingers, interval) = maybe True (interval <=) (Map.lookup fingers defaultMaxFingerSpread)

-- | Ensures that all played notes are inside a given 'PitchRange'.
constrain_insidePitchRange :: PitchRange -> ChordConstraint
constrain_insidePitchRange (lowest, highest) =
    all (`elem` [lowest..highest]) . map snd . Map.toList

defaultAcceptedPitchRange :: PitchRange
defaultAcceptedPitchRange = (toPitch C 3, toPitch C 6)
