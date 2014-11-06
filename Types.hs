module Types where

import qualified Data.Map as Map
import Data.List (sort)

-- | MIDI Note Number
type Pitch = Int
-- | MIDI Octave
type Octave = Int
-- | Used for relative offsets and intervals
type Semitones = Int
-- | A range of 'Pitch'es between a lower and an upper bound
type PitchRange = (Pitch, Pitch)
-- | A chord as a set of 'Pitch'es, without 'Finger' mapping.
-- In this form, the list of 'Pitch'es must be sorted (ascending).
newtype Chord = Chord [Pitch]
    deriving (Eq, Show)
-- | Finger numbers according to piano standards, where the thumb is 1,
-- regardless of hand.
type Finger = Int
-- | A chord with corresponding finger mapping.
type Hand = Map.Map Finger Pitch

-- | The 12 pitch classes in western tonality, without enharmonic equivalents.
data PitchClass = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
    deriving (Show, Eq, Enum, Bounded, Ord)

pitchClasses :: [PitchClass]
pitchClasses = [minBound..maxBound]
numberOfPitchClasses, semitonesPerOctave :: Int
numberOfPitchClasses = length pitchClasses
semitonesPerOctave = numberOfPitchClasses

data MajMin = Major | Minor
    deriving (Show, Enum, Bounded)

-- | Tensions as found in jazz / popular music notation.
data Tension = Seventh | MajorSeventh
             | DiminishedNinth | Ninth | AugmentedNinth
             | Eleventh | AugmnentedEleventh
             | DiminishedThirteenth | Thirteenth
    deriving (Show, Eq, Enum, Bounded)

-- | Converts a 'Tension' to a relative semitone offset that can be added to the root note.
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

-- | Converts from a relative semitone offset to a 'Tension', if there's a corresponding one.
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

-- | A chord symbol as found in jazz / popular music.
-- Consists of a root 'PitchClass', an optional gender, and zero or more 'Tension's.
data ChordSymbol = ChordSymbol {
    chordPitchClass :: PitchClass,
    chordMajMin :: Maybe MajMin,
    chordTensions :: [Tension]
} deriving (Show)

-- | Create a 'Pitch' from a 'PitchClass' and an 'Octave'.
toPitch :: PitchClass -> Octave -> Pitch
toPitch pitchClass octave = (semitonesPerOctave * octave) + fromEnum pitchClass

-- | Retrieve the 'PitchClass' from a 'Pitch', discarding the 'Octave'.
toPitchClass :: Pitch -> PitchClass
toPitchClass = toEnum . snd . (`divMod` semitonesPerOctave)

-- | This should be used to create 'Chord's (instead of the value constructor).
chord :: [Pitch] -> Chord
chord = Chord . sort

-- | Converts from a pure semitones form to a finger-mapped form.
-- Normally, for the left hand, the 5th finger plays the lowest pitch,
-- and the 1st finger plays the highest.
-- If the chord contains less than 5 pitches, the "top" fingers (i.e. 1, 2, ...)
-- will be unused. This could be improved to better distribute the pitches across a hand.
toHand :: Chord -> Hand
toHand (Chord c) = Map.fromList $ zip fingers c
    where fingers = [5, 4, 3, 2, 1] -- Left hand. Right hand would be the reverse.

-- | Converts from finger-mapped form to pure semitone form.
-- The information which finger plays which pitch is lost.
toChord :: Hand -> Chord
toChord = chord . map snd . Map.toList

-- | The interval between two pitches (always positive)
absInterval :: Pitch -> Pitch -> Semitones
absInterval pitch1 pitch2 = abs (pitch1 - pitch2)

-- | The absolute interval between the lowest and the highest pitch of a chord.
-- 0 if there's no interval in the chord.
chordSpan :: Chord -> Semitones
chordSpan c = case c of
    Chord (st:st':sts) -> absInterval st (last $ st':sts)
    _ -> 0
