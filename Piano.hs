module Piano where

import Types
import qualified Data.Map as Map
import Data.Ratio

blackKeys :: [PitchClass]
blackKeys = [Csharp, Dsharp, Fsharp, Gsharp, Asharp]

isBlackKey, isWhiteKey :: Pitch -> Bool
isBlackKey pitch = toPitchClass pitch `elem` blackKeys
isWhiteKey = not . isBlackKey

-- | Check whether the hand is forced to move "into the keys", i.e. the 1st or 5th finger plays a black key.
handIsIntoTheKeys :: Hand -> Bool
handIsIntoTheKeys hand = isBlack (finger 1) || isBlack (finger 5)
    where isBlack = maybe False isBlackKey
          finger = flip Map.lookup hand

-- | The maximum allowed 'Distance' between any two 'Finger's of a hand.
-- These are example pitches, and it doesn't matter which keys are used to measure them, because it is translated into the 'Distance' form anyway.
defaultMaxFingerDistance :: Map.Map (Finger, Finger) Distance
defaultMaxFingerDistance = Map.fromList [
    ((1, 2), distance (toPitch C 3, toPitch C 4)),
    ((1, 3), distance (toPitch B 3, toPitch C 4)),
    ((1, 4), distance (toPitch A 3, toPitch C 4)),
    ((1, 5), distance (toPitch C 3, toPitch E 4)), -- The max. span of the whole hand
    ((2, 3), distance (toPitch Dsharp 3, toPitch B 3)),
    ((2, 4), distance (toPitch Gsharp 3, toPitch F 4)),
    ((2, 5), distance (toPitch C 3, toPitch C 4)),
    ((3, 4), distance (toPitch Asharp 3, toPitch E 4)),
    ((3, 5), distance (toPitch F 3, toPitch Dsharp 4)),
    ((4, 5), distance (toPitch B 3, toPitch E 4))
    ]

-- | 2 is the distance between two white keys.
type Distance = Rational

-- | Calculate the distance of a 'PitchRange' on the keyboard. This normalizes the fact that some semitone intervals on the keyboard (E-F and B-C) are further apart than the others.
distance :: PitchRange -> Distance
distance (p1, p2)
    | p1 == p2 = 0
    | otherwise = neighbourDistance (toPitchClass lower) (toPitchClass next) + distance (next, higher)
    where lower = min p1 p2
          higher = max p1 p2
          next = succ lower

-- | All 'Semitones' intervals have a 'Distance' of 1, except for (E,F) and (B,C), which have a 'Distance' of 2.
-- The first argument has to be the key right below the second argument key.
neighbourDistance :: PitchClass -> PitchClass -> Distance
neighbourDistance pc1 pc2 = defaultTo1 $ Map.lookup (pc1, pc2) exceptions
    where exceptions = Map.union adjacentWhiteKeys nudgedBlackKeys
          defaultTo1 = maybe 1 id
          adjacentWhiteKeys = Map.fromList [((E, F), 2), ((B, C), 2)]
          nudgedBlackKeys = Map.fromList [
            ((C, Csharp), 1 - blackKeyNudgeAmount),
            ((Csharp, D), 1 + blackKeyNudgeAmount),
            ((D, Dsharp), 1 + blackKeyNudgeAmount),
            ((Dsharp, E), 1 - blackKeyNudgeAmount),
            ((F, Fsharp), 1 - blackKeyNudgeAmount),
            ((Fsharp, G), 1 + blackKeyNudgeAmount),
            ((A, Asharp), 1 + blackKeyNudgeAmount),
            ((Asharp, B), 1 - blackKeyNudgeAmount)
            ]

-- | The amount how much black keys are nudged to one side. A value of 1 would mean that they were nudged all the way to the middle of the white key.
blackKeyNudgeAmount :: Rational
blackKeyNudgeAmount = (distanceBetweenFSharpAndGSharp - whiteKeyWidth) % whiteKeyWidth
    -- These are (millimeter) values measured on a piano:
    where distanceBetweenFSharpAndGSharp = 27
          whiteKeyWidth = 23

isAllowedDistance :: ((Finger, Finger), Distance) -> Bool
isAllowedDistance (fingers, dist) = maybe True (dist <=) (Map.lookup fingers defaultMaxFingerDistance)

-- | The absolute interval between the lowest and the highest pitch played by a 'Hand'.
-- 0 if there's no interval in the chord.
handSpan :: Hand -> Distance
handSpan hand = distance (lowestNote hand, highestNote hand)
