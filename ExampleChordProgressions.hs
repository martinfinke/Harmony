module ExampleChordProgressions where

import Types

-- Some abbreviations for common chords
major, minor :: PitchClass -> [Tension] -> ChordSymbol
major pitchClass tensions = ChordSymbol pitchClass (Just Major) tensions Nothing
minor pitchClass tensions = ChordSymbol pitchClass (Just Minor) tensions Nothing

minorSixth, majorSixth, majorSeventh, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminishedSeventh :: PitchClass -> ChordSymbol
minorSixth = flip minor [Sixth]
majorSixth = flip major [Sixth]
majorSeventh = flip major [MajorSeventh]
minorSeventh = flip minor [Seventh]
dominantSeventh = flip major [Seventh]
diminishedSeventh = flip minor [DiminishedFifth, DiminishedSeventh]
halfDiminishedSeventh = flip minor [Seventh, DiminishedFifth]
seventhFlatNine = flip major [Seventh, DiminishedNinth]

repeatPhrase :: Int -> [ChordSymbol] -> [ChordSymbol]
repeatPhrase times chordSymbols
    | times <= 0 = chordSymbols
    | otherwise = chordSymbols ++ repeatPhrase (pred times) chordSymbols

repeatTwice :: [ChordSymbol] -> [ChordSymbol]
repeatTwice = repeatPhrase 2

-- Chord Progressions from The Real Book, 6th Edition (ISBN 0-634-06038-4)

-- All Of Me (Simons/Marks)
allOfMe :: [ChordSymbol]
allOfMe = [
    majorSixth C, dominantSeventh E,
    dominantSeventh A, minorSeventh D,
    dominantSeventh E, minorSeventh A,
    dominantSeventh D, minorSeventh D, dominantSeventh G,
    majorSixth C, dominantSeventh E,
    dominantSeventh A, minorSeventh D,
    majorSixth F, minorSixth F, majorSeventh C, (halfDiminishedSeventh E){chordSlash=Just bFlat}, dominantSeventh A,
    minorSeventh D, dominantSeventh G, majorSixth C, diminishedSeventh eFlat, minorSeventh D, dominantSeventh G
    ]

-- All The Things You Are (Kern / Hammerstein II)
allTheThingsYouAre :: [ChordSymbol]
allTheThingsYouAre = [
    major Csharp [Seventh, AugmentedNinth], major C [Seventh, AugmentedNinth],
    minorSeventh F, minorSeventh bFlat, dominantSeventh eFlat, majorSeventh aFlat,
    majorSeventh dFlat, dominantSeventh G, majorSeventh C,
    minorSeventh C, minorSeventh F, dominantSeventh bFlat, majorSeventh eFlat,
    majorSeventh aFlat, halfDiminishedSeventh A, dominantSeventh D, majorSeventh G, major E [Seventh, AugmentedNinth],
    minorSeventh A, dominantSeventh D, majorSeventh G,
    halfDiminishedSeventh Fsharp, dominantSeventh B, majorSeventh E, major C [Seventh, AugmentedFifth],
    minorSeventh F, minorSeventh bFlat, dominantSeventh eFlat, majorSeventh aFlat,
    majorSeventh dFlat, major gFlat [Seventh, Thirteenth], minorSeventh C, diminishedSeventh B,
    minorSeventh bFlat, dominantSeventh eFlat, majorSeventh aFlat, halfDiminishedSeventh G, seventhFlatNine C
    ]

-- Autumn Leaves (Kosma / Mercer / Prevert)
-- Lots of II-V-I.
autumnLeaves :: [ChordSymbol]
autumnLeaves = [
    minorSeventh A, dominantSeventh D, majorSeventh G,
    majorSeventh C, halfDiminishedSeventh Fsharp, dominantSeventh B, eMinor,
    halfDiminishedSeventh Fsharp, seventhFlatNine B, eMinor,
    minorSeventh A, dominantSeventh D, majorSeventh G,
    halfDiminishedSeventh Fsharp, seventhFlatNine B, minorSeventh E, dominantSeventh A, minorSeventh D, dominantSeventh G,
    halfDiminishedSeventh Fsharp, seventhFlatNine B, eMinor
    ]
    where eMinor = minor E []
    
-- Blue In Green (Davis)
-- Contains "unusual" (i.e. modal) tensions.
blueInGreen :: [ChordSymbol]
blueInGreen = [
    minorSeventh G, major A [Seventh, AugmentedNinth], minorSeventh D, major dFlat [Seventh, DiminishedFifth], minorSeventh C, major F [DiminishedNinth],
    major bFlat [MajorSeventh, AugmentedEleventh], major A [Seventh, AugmentedNinth], minorSeventh D,
    major E [AugmentedFifth, AugmentedNinth], minorSeventh A,
    minorSeventh D, minorSeventh G, major A [Seventh, AugmentedNinth], minorSixth D
    ]

-- A Child is Born (Jones)
-- Contains lots of repetition, and slash chords.
aChildIsBorn :: [ChordSymbol]
aChildIsBorn =
    repeatTwice [majorSeventh bFlat, minorSixth eFlat] ++
    [majorSeventh bFlat, minorSixth eFlat, major bFlat [], halfDiminishedSeventh A, major D [Seventh, AugmentedNinth]] ++
    repeatTwice [minorSeventh G, seventhFlatNine D] ++
    [minorSeventh G, dominantSeventh C, (minorNinth C){chordSlash=Just F}, dominantSeventh F] ++
    repeatTwice [majorSeventh bFlat, (minorSixth eFlat){chordSlash=Just bFlat}] ++
    [majorSeventh bFlat, major D [Seventh, DiminishedFifth, AugmentedNinth], majorSeventh eFlat, major aFlat [Ninth], (halfDiminishedSeventh C){chordSlash=Just gFlat},
    (majorSeventh bFlat){chordSlash=Just F}, (minorSixth eFlat){chordSlash=Just gFlat}, minorSeventh G, dominantSeventh C,
    (minorNinth C){chordSlash=Just F}, dominantSeventh F] ++
    repeatTwice [majorSeventh bFlat, (minorSixth eFlat){chordSlash=Just bFlat}] ++ [majorSeventh bFlat]
    where minorNinth = flip minor [Seventh, Ninth]

-- Donna Lee (Parker)
-- Classic Be-Bop. Most transitions should be possible without jumping.
donnaLee :: [ChordSymbol]
donnaLee = [
    majorSeventh aFlat, dominantSeventh F, dominantSeventh bFlat,
    minorSeventh bFlat, dominantSeventh eFlat, majorSeventh aFlat, minorSeventh eFlat, dominantSeventh D,
    majorSeventh dFlat, minorSeventh dFlat, dominantSeventh gFlat, majorSeventh aFlat, seventhFlatNine F,
    dominantSeventh bFlat, minorSeventh bFlat, dominantSeventh eFlat,
    majorSeventh aFlat, dominantSeventh F, dominantSeventh bFlat,
    halfDiminishedSeventh G, seventhFlatNine C, minorSeventh F, dominantSeventh C,
    minorSeventh F, halfDiminishedSeventh G, dominantSeventh C, minor F [], dominantSeventh bFlat, diminishedSeventh B,
    minorSeventh C, dominantSeventh F, minorSeventh bFlat, dominantSeventh eFlat, majorSeventh aFlat, seventhFlatNine F, minorSeventh bFlat, dominantSeventh eFlat
    ]

-- Epistrophy (Monk / Clarke)
-- Unusual/chromatic (Monk-style) progressions. And LOTS of repetition
epistrophy :: [ChordSymbol]
epistrophy =
    repeatPhrase 4 p1 ++
    repeatPhrase 8 p2 ++
    repeatPhrase 4 p1 ++
    [minorSixth Fsharp, dominantSeventh B, dominantSeventh dFlat, dominantSeventh D] ++
    repeatPhrase 4 p2 ++
    repeatPhrase 4 p1 ++
    p1 ++ [major gFlat [Seventh, AugmentedEleventh]]
    where p1 = [dominantSeventh Csharp, dominantSeventh D]
          p2 = [dominantSeventh Dsharp, dominantSeventh E]


-- Giant Steps (Coltrane)
giantSteps :: [ChordSymbol]
giantSteps = [
    majorSeventh B, dominantSeventh D, majorSeventh G, dominantSeventh bFlat, majorSeventh eFlat, minorSeventh A, dominantSeventh D,
    majorSeventh G, dominantSeventh bFlat, majorSeventh eFlat, dominantSeventh Fsharp, majorSeventh B, minorSeventh F, dominantSeventh bFlat,
    majorSeventh eFlat, minorSeventh A, dominantSeventh D, majorSeventh G, minorSeventh Csharp, dominantSeventh Fsharp,
    majorSeventh B, minorSeventh F, dominantSeventh bFlat, majorSeventh eFlat, minorSeventh Csharp, dominantSeventh Fsharp
    ]

-- God Bless The Child (Herzog, Holiday)
godBlessTheChild :: [ChordSymbol]
godBlessTheChild =
    p1 ++ [minorSeventh F, dominantSeventh bFlat] ++
    p1 ++ [halfDiminishedSeventh D, dominantSeventh G] ++
    p2 ++ [halfDiminishedSeventh D, dominantSeventh G] ++
    p2 ++ [dominantSeventh C, halfDiminishedSeventh F, dominantSeventh bFlat] ++
    p1 ++ [minorSeventh F, dominantSeventh bFlat]
    where p1 = repeatTwice [majorSeventh eFlat, dominantSeventh eFlat, majorSixth aFlat] ++
               repeatTwice [minorSeventh bFlat, dominantSeventh eFlat] ++
               [majorSeventh aFlat, majorSixth aFlat, minor aFlat [MajorSeventh], minorSixth aFlat, minorSeventh G, seventhFlatNine C,
               minorSeventh F, dominantSeventh bFlat, majorSixth eFlat]
          p2 = [minor C [], minor C [MajorSeventh], minorSeventh C, minorSixth C, minorSeventh G]
