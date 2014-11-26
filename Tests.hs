{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)
import Test.QuickCheck (Arbitrary(arbitrary), Gen, choose, oneof, quickCheck, verboseCheck, resize, listOf1)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import Data.List (sort)
import qualified Data.Map as Map

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
newtype UnsignedSmallInt = UnsignedSmallInt Int
    deriving (Eq, Show)
instance Arbitrary UnsignedSmallInt where
    arbitrary = fmap UnsignedSmallInt $ choose (0, snd integerTestRange)
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

instance Arbitrary Chord where
    arbitrary = chord `fmap` (resize 5 randomPitches)
        where randomPitches = listOf1 arbitrary :: Gen [Pitch]

-- Types Tests
prop_tensionToSemitones_symmetry tension =
    (Just tension) == (semitonesToTension . tensionToSemitones) tension

prop_tensionToSemitones_modulus (SmallInt semitones) =
    let (_, semitonesModOctave) = semitones `divMod` semitonesPerOctave
        tension1 = semitonesToTension semitones
        tension2 = semitonesToTension semitonesModOctave
    in tension1 == tension2

prop_ChordToHand_symmetry (UnsignedSmallInt pitch1) (UnsignedSmallInt pitch2) (UnsignedSmallInt pitch3) =
    let c = chord [pitch1, pitch2, pitch3]
    in c == (toChord . toHand) c

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
