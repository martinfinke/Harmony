module Types(
             Pitch,
             Octave,
             Semitones,
             PitchRange,
             Chord(..),
             Finger,
             Hand,
             PitchClass(..),
             semitonesPerOctave,
             MajMin(..),
             Tension(..),
             ChordSymbol(..),
             chord,
             toPitch,
             toPitchClass,
             toHand,
             toChord,
             absInterval,
             chordSpan,
             tensionToSemitones,
             semitonesToTension,
             pitchClasses) where

import qualified Data.Map as Map
import Data.List (sort)

type Pitch = Int
type Octave = Int
type Semitones = Int
type PitchRange = (Pitch, Pitch) -- min/max
newtype Chord = Chord [Pitch]
    deriving (Eq, Show)
type Finger = Int
type Hand = Map.Map Finger Pitch

data PitchClass = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
    deriving (Show, Eq, Enum, Bounded, Ord)

pitchClasses :: [PitchClass]
pitchClasses = [minBound..maxBound]
numberOfPitchClasses, semitonesPerOctave :: Int
numberOfPitchClasses = length pitchClasses
semitonesPerOctave = numberOfPitchClasses

data MajMin = Major | Minor
    deriving (Show, Enum, Bounded)

data Tension = Seventh | MajorSeventh
             | DiminishedNinth | Ninth | AugmentedNinth
             | Eleventh | AugmnentedEleventh
             | DiminishedThirteenth | Thirteenth
    deriving (Show, Eq, Enum, Bounded)

tensionToSemitones :: Tension -> Semitones
tensionToSemitones tension = snd . (`divMod` semitonesPerOctave) $ case tension of
    Seventh -> 10
    MajorSeventh -> 11
    DiminishedNinth -> 13
    Ninth -> 14
    AugmentedNinth -> 15
    Eleventh -> 17
    AugmnentedEleventh -> 18
    DiminishedThirteenth -> 20
    Thirteenth -> 21

semitonesToTension :: Semitones -> Maybe Tension
semitonesToTension semitones = case snd $ semitones `divMod` semitonesPerOctave of
    1 -> Just DiminishedNinth
    2 -> Just Ninth
    3 -> Just AugmentedNinth
    5 -> Just Eleventh
    6 -> Just AugmnentedEleventh
    8 -> Just DiminishedThirteenth
    9 -> Just Thirteenth
    10 -> Just Seventh
    11 -> Just MajorSeventh
    _ -> Nothing

data ChordSymbol = ChordSymbol {
    chordPitchClass :: PitchClass,
    chordMajMin :: Maybe MajMin,
    chordTensions :: [Tension]
} deriving (Show)

toPitch :: PitchClass -> Octave -> Pitch
toPitch pitchClass octave = (semitonesPerOctave * octave) + fromEnum pitchClass

toPitchClass :: Pitch -> PitchClass
toPitchClass = toEnum . snd . (`divMod` semitonesPerOctave)

chord :: [Pitch] -> Chord
chord = Chord . sort

-- | Converts from a pure semitones form to a finger-mapped form.
-- | Normally, for the left hand, the 5th finger plays the lowest pitch,
-- | and the 1st finger plays the highest.
-- | If the chord contains less than 5 pitches, the "top" fingers (i.e. 1, 2, ...)
-- | will be unused. This could be improved to better distribute the pitches.
toHand :: Chord -> Hand
toHand (Chord c) = Map.fromList fingerMappings
    where fingerMappings = zip fingers c
          fingers = [5, 4, 3, 2, 1] -- Left hand. Right hand would be the reverse.

-- | Converts from finger-mapped form to pure semitone form.
-- | The information which finger plays which pitch is lost.
toChord :: Hand -> Chord
toChord = chord . map snd . Map.toList

absInterval :: Pitch -> Pitch -> Semitones
absInterval pitch1 pitch2 = abs (pitch1 - pitch2)

-- | The interval between the lowest and the highest pitch of a chord.
chordSpan :: Chord -> Semitones
chordSpan c = case c of
    Chord (st:st':sts) -> absInterval st (last $ st':sts)
    _ -> 0
