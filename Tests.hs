{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)
import Test.QuickCheck (Arbitrary(arbitrary), Gen, choose, oneof, quickCheck, verboseCheck)
import System.Exit (ExitCode, exitFailure, exitSuccess)

import Types
import GenerateChords

-- | Generate an arbitrary element from a list
chooseOne :: [a] -> Gen a
chooseOne = oneof . map return

-- | Wrapper type to generate integer values inside a predefined range.
-- | Prevents overflow when QuickCheck tests random values.
newtype SmallInt = SmallInt Int
    deriving (Eq, Show)
instance Arbitrary SmallInt where
    arbitrary = fmap SmallInt $ choose (fst integerTestRange, snd integerTestRange)
-- | The low/high bound to use for SmallInt
integerTestRange :: (Int, Int)
integerTestRange = (-1000000, 1000000)

instance Arbitrary PitchClass where
    arbitrary = chooseOne pitchClasses

instance Arbitrary MajMin where
    arbitrary = chooseOne possibleValues
        where possibleValues = [minBound..maxBound] :: [MajMin]

instance Arbitrary Tension where
    arbitrary = chooseOne possibleValues
        where possibleValues = [minBound..maxBound] :: [Tension]

instance Arbitrary Pitch where
    arbitrary = do
        (SmallInt noteNumber) <- arbitrary
        return $ noteNumberToPitch noteNumber


-- Types Tests
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
    let (_, semitonesModOctave) = semitones `divMod` semitonesPerOctave
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



-- GenerateChords Tests
prop_combinations_correctLength semitones lengthLimit =
    let maxLength = 5
        (_, lengthLimit') = lengthLimit `divMod` maxLength
        semitones' = take lengthLimit' semitones
    in (length . combinations) semitones' == (length allowedOctaveTranspositions)^(length semitones')



-- needed by QuickCheck
return []
main :: IO ExitCode
main = do
    success <- runTests
    if success then exitSuccess else exitFailure
runTests, runTestsVerbose :: IO Bool
runTests = $quickCheckAll
runTestsVerbose = $verboseCheckAll
