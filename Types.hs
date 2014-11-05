module Types where

type NoteNumber = Int
type Octave = Int
type Semitones = Int
type PitchRange = (Pitch, Pitch) -- min/max
type Chord = [Pitch]

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

data Pitch = Pitch PitchClass Octave
    deriving (Show, Eq)
instance Ord Pitch where
    (Pitch pitchClass1 octave1) `compare` (Pitch pitchClass2 octave2) =
        if octave1 == octave2
        then pitchClass1 `compare` pitchClass2
        else octave1 `compare` octave2
instance Enum Pitch where
    toEnum = noteNumberToPitch
    fromEnum = pitchToNoteNumber

pitchToNoteNumber :: Pitch -> NoteNumber
pitchToNoteNumber (Pitch pitchClass octave) = (semitonesPerOctave * octave) + fromEnum pitchClass

noteNumberToPitch :: NoteNumber -> Pitch
noteNumberToPitch number =
    let (octave, rest) = number `divMod` semitonesPerOctave
        pitchClass = toEnum rest
    in Pitch pitchClass octave

-- | The interval from one pitch to another
interval :: Pitch -> Pitch -> Semitones
interval note1 note2 = (pitchToNoteNumber note2) - (pitchToNoteNumber note1)

-- | Transpose a pitch by a number of semitones
transpose :: Semitones -> Pitch -> Pitch
transpose semitones = noteNumberToPitch . (+ semitones) . pitchToNoteNumber

-- | Converts a (relative) semitone offset to an (absolute) pitch, given a root pitch.
semitoneToPitch :: Pitch -> Semitones -> Pitch
semitoneToPitch = flip transpose

findNextNoteBelow, findNextNoteAbove :: PitchClass -> Pitch -> Pitch
findNextNoteBelow = findNote pred
findNextNoteAbove = findNote succ

findNote :: (Pitch -> Pitch) -> PitchClass -> Pitch -> Pitch
findNote directionFunction  pitchClassToFind currentNote@(Pitch pitchClass _)
    | pitchClass == pitchClassToFind = currentNote
    | otherwise = findNote directionFunction pitchClassToFind $ directionFunction currentNote

-- | Checks whether all notes of a chord are inside a pitch range
chordIsInsidePitchRange :: PitchRange -> Chord -> Bool
chordIsInsidePitchRange range = all $ noteIsInsidePitchRange range


noteIsInsidePitchRange :: PitchRange -> Pitch -> Bool
noteIsInsidePitchRange (minNote, maxNote) note
    | minNote <= note && note <= maxNote = True
    | otherwise = False
