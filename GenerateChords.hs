{-|
Module      : GenerateChords
Description : Generates all possible 'Hand' progressions for a given 'ChordSymbol' progression.

The result is a 'State' graph that can be used by the 'Optimize' module to find the optimal progression.
-}
module GenerateChords where

import Types
import Data.List (nub, sortBy)
import Data.Ord (comparing)

-- | Creates the basic relative semitone offsets for a triad (root, third, fifth).
basicSemitones :: ChordSymbol -> [Semitones]
basicSemitones (ChordSymbol pitchClass maybeMajMin tensions maybeSlash) =case maybeMajMin of
    Just Major -> [root, majorThird, fifth] ++ slashSemitones
    Just Minor -> [root, minorThird, fifth] ++ slashSemitones
    -- Doesn't create a third if the 'ChordSymbol' is neither major nor minor:
    Nothing -> [root, fifth] ++ slashSemitones
    where root = 0
          minorThird = 3
          majorThird = 4
          -- Create a diminished fifth (instead of the perfect fifth), if specified:
          fifth = if DiminishedFifth `elem` tensions then 6 else 7
          -- Generate a semitone offset for the slash note:
          slashSemitones = case maybeSlash of
              Nothing -> []
              Just slash -> [pitchClassInterval pitchClass slash]

-- | Adds relative semitone offsets for tensions.
addTensions :: [Tension] -> [Semitones] -> [Semitones]
addTensions tensions sts = sts ++ map tensionToSemitones tensions

-- | All possible ways to layer the pitches that belong to a given chord symbol.
chordsForChordSymbol :: ChordSymbol -> [Chord]
chordsForChordSymbol chordSymbol@(ChordSymbol pitchClass _ tensions slash) = case slash of
    Nothing -> absolutePitches
    Just slashPitchClass -> filter (lowestNoteIs slashPitchClass) absolutePitches
    where 
          -- Create the relative semitone offsets inside the chord:
          relativeSemitones = (nub . addTensions tensions . basicSemitones) chordSymbol
          -- Generate all possible layerings by transposing offsets up/down:
          semitoneCombinations = combinations relativeSemitones
          -- Convert the relative offsets to absolute pitches:
          absolutePitches = map toAbsolutePitch semitoneCombinations
          toAbsolutePitch = chord . map (+ toPitch pitchClass octave)
          -- The base octave from which the offsets will be transposed:
          octave = 3 -- TODO: Should be a parameter

-- | 'Hand' version of 'chordsForChordSymbol'
handsForChordSymbol :: ChordSymbol -> [Hand]
handsForChordSymbol = map toHand . chordsForChordSymbol

-- | Checks whether the lowest note in the 'Chord' is a given 'PitchClass'.
-- Assumes that the 'Chord' is sorted (i.e. the slash note should be the first element)
lowestNoteIs :: PitchClass -> Chord -> Bool
lowestNoteIs slashPitchClass (Chord c) = case c of
    [] -> True
    slash:_ -> toPitchClass slash == slashPitchClass

-- | All possible ways to layer a set of semitones.
combinations :: [Semitones] -> [[Semitones]]
combinations [] = [[]]
combinations (st:sts) = concat $ map appendRest transposedSts
    where transposedSts = map (+st) allowedOctaveTranspositions
          appendRest semitones = map (semitones:) (combinations sts)

-- | Determines how much a note in a chord may be transposed up or down.
-- This list should only contain octaves (i.e. multiples of 12).
allowedOctaveTranspositions :: [Semitones]
allowedOctaveTranspositions = map (*semitonesPerOctave) [-1, 0, 1]

