module GenerateChords where

import Types
import Data.List (nub)

-- | Creates the basic relative semitone offsets for a triad (root, third, fifth).
-- | Doesn't create a third if neither major nor minor.
basicSemitones :: ChordSymbol -> [Semitones]
basicSemitones (ChordSymbol _ maybeMajMin _) = case maybeMajMin of
    Just Major -> [root, majorThird, fifth]
    Just Minor -> [root, minorThird, fifth]
    Nothing -> [root, fifth]
    where root = 0
          minorThird = 3
          majorThird = 4
          fifth = 7

-- | Adds relative semitone offsets for tensions.
addTensions :: [Tension] -> [Semitones] -> [Semitones]
addTensions tensions c = c ++ map tensionToSemitones tensions

-- | All possible ways to layer the pitches that belong to a given chord symbol.
chordsForChordSymbol :: ChordSymbol -> [Chord]
chordsForChordSymbol chordSymbol@(ChordSymbol pitchClass _ tensions) =
    map toAbsolutePitch semitoneCombinations
    where normalizedSemitones = (nub . addTensions tensions . basicSemitones) chordSymbol
          semitoneCombinations = combinations normalizedSemitones
          toAbsolutePitch = chord . map (+ toPitch pitchClass octave)
          octave = 3

-- | All possible ways to layer a set of semitones.
combinations :: [Semitones] -> [[Semitones]]
combinations [] = [[]]
combinations (st:sts) = concat $ map appendRest transposedSts
    where transposedSts = map (+st) allowedOctaveTranspositions
          appendRest semitones = map (semitones:) (combinations sts)

allowedOctaveTranspositions :: [Semitones]
allowedOctaveTranspositions = map (*semitonesPerOctave) [-1, 0, 1]

