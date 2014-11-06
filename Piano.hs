module Piano where

import Types
import qualified Data.Map as Map

blackKeys :: [PitchClass]
blackKeys = [Csharp, Dsharp, Fsharp, Gsharp, Asharp]

isBlackKey, isWhiteKey :: Pitch -> Bool
isBlackKey pitch = toPitchClass pitch `elem` blackKeys
isWhiteKey = not . isBlackKey

defaultMaxFingerSpread :: Map.Map (Finger, Finger) Semitones
defaultMaxFingerSpread = Map.fromList [
    ((1, 2), 12), -- 1st and 2nd finger can span an octave
    ((2, 3), 4), -- max. 4 semitones from 2nd to 3rd finger
    ((3, 4), 4),
    ((4, 5), 4),
    ((2, 5), 11),
    ((1, 5), 16) -- max. a sharp decime with the whole hand
    ]
