module ConstrainChord where

import Types
import Piano
import qualified Data.Map as Map
import Data.List (nub)

type ChordConstraint = Hand -> Bool

constrain_fingerSpreading :: ChordConstraint
constrain_fingerSpreading hand =
    all checkFinger $ Map.toList hand
    where checkFinger (finger, pitch) =
            let others = Map.delete finger hand
                checkInterval (fingers, interval) = maybe True (interval <=) $ Map.lookup fingers defaultMaxFingerSpread
                combine (finger', pitch') = ((min finger finger', max finger finger'), absInterval pitch pitch')
                intervals = map combine $ Map.toList others
            in all checkInterval intervals

