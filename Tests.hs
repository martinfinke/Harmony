{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)
import Test.QuickCheck (Arbitrary(arbitrary), choose, oneof, quickCheck, verboseCheck)
import System.Exit (ExitCode, exitFailure, exitSuccess)

import Types

instance Arbitrary PitchClass where
    arbitrary = oneof . map return $ pitchClasses

newtype SmallInt = SmallInt Int deriving (Eq, Show)
instance Arbitrary SmallInt where
    arbitrary = fmap SmallInt $ choose (-maxValue, maxValue)
        where maxValue = 1000000

instance Arbitrary MajMin where
    arbitrary = oneof . map return $ possibleValues
        where possibleValues = [minBound..maxBound] :: [MajMin]

instance Arbitrary Tension where
    arbitrary = oneof . map return $ possibleValues
        where possibleValues = [minBound..maxBound] :: [Tension]

instance Arbitrary Pitch where
    arbitrary = do
        (SmallInt noteNumber) <- arbitrary
        return $ noteNumberToPitch noteNumber

prop_fromNoteNumber (SmallInt noteNumber) =
    (pitchToNoteNumber . noteNumberToPitch) noteNumber == noteNumber

prop_transposePlusMinus (SmallInt amount) (SmallInt noteNumber) =
    (transpose (-amount) . transpose amount) note == note
    where note = noteNumberToPitch noteNumber

prop_transposeNoteNumber (SmallInt amount) (SmallInt noteNumber) =
    transpose amount note == transposedNote
    where note = noteNumberToPitch noteNumber
          transposedNote = noteNumberToPitch (noteNumber + amount)

prop_intervalTranspose (SmallInt noteNumber1) (SmallInt noteNumber2) =
    interval note1 note2 == noteNumber2 - noteNumber1
    where note1 = noteNumberToPitch noteNumber1
          note2 = noteNumberToPitch noteNumber2

prop_noteIsInsideNoteRange note minNote (SmallInt range) =
    let maxNote = transpose (abs range) minNote
        isInsideRange = minNote <= note
                      && note <= maxNote
    in isInsideRange == noteIsInsidePitchRange (minNote, maxNote) note

prop_noteOrdering note1 note2 =
    note1 `compare` note2 == pitchToNoteNumber note1 `compare` pitchToNoteNumber note2

prop_tensionToSemitones_symmetry tension =
    (Just tension) == (semitonesToTension . tensionToSemitones) tension

prop_tensionToSemitones_modulus (SmallInt semitones) =
    let (_, semitonesModOctave) = semitones `divMod` notesPerOctave
        tension1 = semitonesToTension semitones
        tension2 = semitonesToTension semitonesModOctave
    in tension1 == tension2

prop_findNote_correctPitchClass pitchClass startingNote =
    let (Pitch resultPitchClass _) = findNextNoteBelow pitchClass startingNote
    in resultPitchClass == pitchClass

prop_findNote_isBelowStartingNote pitchClass startingNote =
    findNextNoteBelow pitchClass startingNote <= startingNote

prop_findNote_isAboveStartingNote pitchClass startingNote =
    findNextNoteAbove pitchClass startingNote >= startingNote

-- needed by QuickCheck
return []
main :: IO ExitCode
main = do
    success <- runTests
    if success then exitSuccess else exitFailure
runTests, runTestsVerbose :: IO Bool
runTests = $quickCheckAll
runTestsVerbose = $verboseCheckAll
