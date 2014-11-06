module ConstrainChord where

import Types
import Piano
import Utils (combinePairsWith, sortTuple)
import qualified Data.Map as Map

type ChordConstraint = Hand -> Bool

-- | Ensures that no two fingers in a hand exceed their max spread (Piano.defaultMaxFingerSpread).
constrain_fingerSpreading :: ChordConstraint
constrain_fingerSpreading hand =
    all checkInterval intervals
    where combineFingerMappings (f, p) (f', p') = (sortTuple (f, f'), absInterval p p')
          intervals = combinePairsWith combineFingerMappings $ Map.toList hand
          checkInterval (fingers, interval) = maybe True (interval <=) (Map.lookup fingers defaultMaxFingerSpread)


