{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)
import Test.QuickCheck (Arbitrary(arbitrary), Gen, choose, oneof, quickCheck, verboseCheck)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import Data.List (sort)

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




        

-- Types Tests
prop_tensionToSemitones_symmetry tension =
    (Just tension) == (semitonesToTension . tensionToSemitones) tension

prop_tensionToSemitones_modulus (SmallInt semitones) =
    let (_, semitonesModOctave) = semitones `divMod` semitonesPerOctave
        tension1 = semitonesToTension semitones
        tension2 = semitonesToTension semitonesModOctave
    in tension1 == tension2

prop_ChordToHand_symmetry (SmallInt pitch1) (SmallInt pitch2) (SmallInt pitch3) =
    let chord = sort [pitch1, pitch2, pitch3]
    in chord == (toChord . toHand) chord

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
