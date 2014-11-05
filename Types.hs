module Types where

import qualified Data.Map as Map
import Data.List (sort)

type Pitch = Int
type Octave = Int
type Semitones = Int
type PitchRange = (Pitch, Pitch) -- min/max
type Chord = [Pitch]
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

toHand :: Chord -> Hand
toHand chord = Map.fromList pairs
    where pairs = zip fingers chord
          fingers = [5, 4, 3, 2, 1] -- Left hand. Right hand would be the reverse.

toChord :: Hand -> Chord
toChord = sort . map snd . Map.toList

absInterval :: Pitch -> Pitch -> Semitones
absInterval pitch1 pitch2 = abs (pitch1 - pitch2)

chordSpan :: Chord -> Semitones
chordSpan chord = case chord of
    st:st':sts -> absInterval st (last $ st':sts)
    _ -> 0
