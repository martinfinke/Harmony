{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Types
Description : Fundamental data types used throughout the project.
-}
module Types where

import qualified Data.Map as Map
import Data.List (sort, intercalate, sortBy)
import Data.Ord (comparing)
import Data.Tuple (swap)

-- | MIDI Note Number
newtype Pitch = Pitch Int
    deriving (Eq, Show, Ord)

instance Enum Pitch where
    toEnum = Pitch
    fromEnum (Pitch p) = p

class Transposable a b where
   (<+>) :: a -> b -> a
   (<->) :: a -> b -> a

infixl 6 <+>, <->

instance Transposable Pitch Semitones where
    (Pitch p) <+> (Semitones sts) = Pitch (p + sts)
    (Pitch p) <-> (Semitones sts) = Pitch (p - sts)

instance Transposable Semitones Semitones where
    (Semitones st1) <+> (Semitones st2) =  Semitones (st1+st2)
    (Semitones st1) <-> (Semitones st2) =  Semitones (st1-st2)

-- | MIDI Octave
newtype Octave = Octave Int
    deriving (Eq, Ord, Show)

-- | Used for relative offsets and intervals
newtype Semitones = Semitones Int
    deriving (Eq, Ord)

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

-- | A 'Rating' is always >= 0, and a low value is better.
type Rating = Float

perfectRating, standardPenalty :: Rating
-- | The rating given to a flawless 'Hand'.
perfectRating = 0.0
-- | The penalty added to a 'Hand' whenever it violates a rule.
standardPenalty = 1.0

-- | A 'Hand' together with its 'totalRating'.
-- The 'Rating' is included because values of this type are duplicated a lot in different 'SubProgression's, and it would be redundant to calculate the 'Rating' again each time.
data RatedHand = RatedHand {hand :: Hand, rating :: Rating}
    deriving(Show)

-- | The 12 pitch classes in western tonality, without enharmonic equivalents.
data PitchClass = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
    deriving (Show, Eq, Enum, Bounded, Ord)

-- | Enharmonics (as aliases for their sharp equivalents)
cFlat, dFlat, eFlat, fFlat, gFlat, aFlat, bFlat :: PitchClass
cFlat = B
dFlat = Csharp
eFlat = Dsharp
fFlat = E
gFlat = Fsharp
aFlat = Gsharp
bFlat = Asharp

pitchClasses :: [PitchClass]
pitchClasses = [minBound..maxBound]

numberOfPitchClasses :: Int
numberOfPitchClasses = length pitchClasses

semitonesPerOctave :: Int
semitonesPerOctave = numberOfPitchClasses

-- | Major/minor of a chord. This determines which kind of third will be generated.
data MajMin = Major | Minor
    deriving (Eq, Show, Enum, Bounded)

-- | Tensions as found in jazz / popular music notation.
data Tension = DiminishedFifth | AugmentedFifth
             | DiminishedSixth | Sixth
             | DiminishedSeventh | Seventh | MajorSeventh
             | DiminishedNinth | Ninth | AugmentedNinth
             | Eleventh | AugmentedEleventh
             | DiminishedThirteenth | Thirteenth
    deriving (Eq, Enum, Bounded, Ord)

instance Show Tension where
    show t = case t of
        DiminishedFifth -> "b5"
        AugmentedFifth -> "#5"
        DiminishedSixth -> "b6"
        Sixth -> "6"
        DiminishedSeventh -> "b7"
        Seventh -> "7"
        MajorSeventh -> "maj7"
        DiminishedNinth -> "b9"
        Ninth -> "9"
        AugmentedNinth -> "#9"
        Eleventh -> "11"
        AugmentedEleventh -> "#11"
        DiminishedThirteenth -> "b13"
        Thirteenth -> "13"

showTensions :: [Tension] -> String
showTensions [] = ""
showTensions tensions = (' ':) . intercalate " " . map show . sort $ tensions


-- | Converts a 'Tension' to a relative semitone offset that can be added to the root note.
tensionToSemitones :: Tension -> Semitones
tensionToSemitones tension = Semitones . snd . (`divMod` semitonesPerOctave) $ case tension of
    DiminishedFifth -> 6
    AugmentedFifth -> 8
    DiminishedSixth -> 8
    Sixth -> 9
    DiminishedSeventh -> 9
    Seventh -> 10
    MajorSeventh -> 11
    DiminishedNinth -> 13
    Ninth -> 14
    AugmentedNinth -> 15
    Eleventh -> 17
    AugmentedEleventh -> 18
    DiminishedThirteenth -> 20
    Thirteenth -> 21

-- | Converts from a relative semitone offset to a 'Tension', if there's a corresponding one.
semitonesToTension :: Semitones -> Maybe Tension
semitonesToTension (Semitones sts) = case snd $ sts `divMod` semitonesPerOctave of
    1 -> Just DiminishedNinth
    2 -> Just Ninth
    3 -> Just AugmentedNinth
    5 -> Just Eleventh
    6 -> Just AugmentedEleventh
    8 -> Just DiminishedThirteenth
    9 -> Just Thirteenth
    10 -> Just Seventh
    11 -> Just MajorSeventh
    _ -> Nothing

-- | A part of the chord progression.
-- 'Start' is a marker for the beginning of the progression, 'End' for the end.
-- The 'SubProgression' value constructor wraps a progression of 'Hand's that will be rated together.
data SubProgression = Start
              | SubProgression [RatedHand]
              | End
    deriving(Show)

-- | A chord symbol as found in jazz / popular music.
-- Consists of a root 'PitchClass', an optional gender, and zero or more 'Tension's.
data ChordSymbol = ChordSymbol {
    chordPitchClass :: PitchClass,
    chordMajMin :: Maybe MajMin,
    chordTensions :: [Tension],
    chordSlash :: Maybe PitchClass
}

instance Show ChordSymbol where
    show (ChordSymbol pitchClass maybeMajMin tensions maybeChordSlash) =
        show pitchClass ++ showMajMin maybeMajMin ++ showChordSlash maybeChordSlash ++ showTensions tensions
        where showMajMin = maybe "" $ \majMin -> if majMin == Minor then "-" else ""
              showChordSlash = maybe "" (('_':) . show)

-- | Create a 'Pitch' from a 'PitchClass' and an 'Octave'.
toPitch :: PitchClass -> Int -> Pitch
toPitch pitchClass octave = Pitch $ (semitonesPerOctave * octave) + fromEnum pitchClass

-- | Inverse of 'toPitch'.
fromPitch :: Pitch -> (PitchClass, Octave)
fromPitch (Pitch pitch) =
    let (octave, pc) = pitch `divMod` semitonesPerOctave
    in (toEnum pc, Octave octave)

-- | Retrieve the 'PitchClass' from a 'Pitch', discarding the 'Octave'.
toPitchClass :: Pitch -> PitchClass
toPitchClass (Pitch pitch) = toEnum . snd . (`divMod` semitonesPerOctave) $ pitch

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
    where fingers = [5, 4, 3, 2, 1] -- Left hand. Right hand would be the 'reverse'.

-- | Converts from finger-mapped form to pure semitone form.
-- The information which finger plays which pitch is lost.
toChord :: Hand -> Chord
toChord = chord . map snd . Map.toList

-- | The lowest and highest note played by a hand. The hand may not be empty.
lowestNote, highestNote :: Hand -> Pitch
highestNote = maximum . map snd . Map.toList
lowestNote = minimum . map snd . Map.toList

-- | The interval "spanned" by a 'Hand' (interval between lowest to highest note).
semitoneSpan :: Hand -> Semitones
semitoneSpan hand = absInterval (lowestNote hand) (highestNote hand)

-- | The interval between two pitches (always positive)
absInterval :: Pitch -> Pitch -> Semitones
absInterval (Pitch pitch1) (Pitch pitch2) = Semitones $ abs (pitch1 - pitch2)

-- | The interval between two pitch classes (always positive)
pitchClassInterval :: PitchClass -> PitchClass -> Semitones
pitchClassInterval pc1 pc2 = absInterval (pitch pc1) (pitch pc2)
    where pitch = (flip toPitch 0)

-- | The pitch range from the first to the second argument. Returns 'Nothing' iff the second pitch is below (or equal to) the first.
pitchRangeBetween :: Pitch -> Pitch -> Maybe PitchRange
pitchRangeBetween p1 p2
    | p2 > p1 = Just (p1, p2)
    | otherwise = Nothing

-- | Whether two 'Pitch'es are next to each other (i.e. one is the 'succ'essor of the other)
areNeighbouring :: Pitch -> Pitch -> Bool
areNeighbouring p1 p2 = succ p1 == p2 || succ p2 == p1

-- | The two pitches directly next to a pitch.
neighbours :: Pitch -> [Pitch]
neighbours p = [pred p, succ p]

-- | Displays all finger-pitch mappings of a 'Hand' in a easy-to-read way.
showHand :: Hand -> String
showHand hand = intercalate "," $ map show $ sortBy (comparing swap) $ map (fromPitch . snd) (Map.toList hand)

