module GenerateChords where

import Types
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import RateChord
import RateChordTransition
import Optimize (State(..))
import Utils (allCombinations)

-- | Creates the basic relative semitone offsets for a triad (root, third, fifth).
-- | Doesn't create a third if neither major nor minor.
basicSemitones :: ChordSymbol -> [Semitones]
basicSemitones (ChordSymbol pitchClass maybeMajMin tensions maybeSlash) =case maybeMajMin of
    Just Major -> [root, majorThird, fifth] ++ slashSemitones
    Just Minor -> [root, minorThird, fifth] ++ slashSemitones
    Nothing -> [root, fifth] ++ slashSemitones
    where root = 0
          minorThird = 3
          majorThird = 4
          fifth = if DiminishedFifth `elem` tensions then 6 else 7
          slashSemitones = case maybeSlash of
              Nothing -> []
              Just slash -> [pitchClassInterval pitchClass slash]

-- | Adds relative semitone offsets for tensions.
addTensions :: [Tension] -> [Semitones] -> [Semitones]
addTensions tensions c = c ++ map tensionToSemitones tensions

-- | All possible ways to layer the pitches that belong to a given chord symbol.
chordsForChordSymbol :: ChordSymbol -> [Chord]
chordsForChordSymbol chordSymbol@(ChordSymbol pitchClass _ tensions slash) = case slash of
    Nothing -> absolutePitches
    Just slashPitchClass -> filter (lowestNoteIs slashPitchClass) absolutePitches
    where normalizedSemitones = (nub . addTensions tensions . basicSemitones) chordSymbol
          semitoneCombinations = combinations normalizedSemitones
          toAbsolutePitch = chord . map (+ toPitch pitchClass octave)
          absolutePitches = map toAbsolutePitch semitoneCombinations
          octave = 3

handsForChordSymbol :: ChordSymbol -> [Hand]
handsForChordSymbol = map toHand . chordsForChordSymbol

-- | Checks whether the lowest note in the 'Chord' is a given 'PitchClass'.
-- Assumes that chord is sorted (i.e. the slash should be the first element)
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

allowedOctaveTranspositions :: [Semitones]
allowedOctaveTranspositions = map (*semitonesPerOctave) [-1, 0, 1]

transitionFromState :: State -> Hand -> (State, Rating)
transitionFromState InitialState _ = (InitialState, perfectRating)
transitionFromState (GoalState _) _ = error "Trying to transition out of GoalState!"
transitionFromState state@(State parentHand _ _) hand =
    (state, totalTransitionRating (parentHand, hand))

-- | Generates a graph of all possible 'Hand' progressions for a list of 'ChordSymbol's
-- Returns a 'GoalState' that can be used by the 'Optimize' module to find the optimal chord progression.
generateProgressions :: [ChordSymbol] -> State
generateProgressions = generateProgressions' [InitialState]

-- | Utility function for recursion
generateProgressions' :: [State] -> [ChordSymbol] -> State
generateProgressions' parentStates (currentSymbol:others) =
    let currentHands = handsForChordSymbol currentSymbol
        currentRatings = map totalRating currentHands
        transitionsFromParent = map (transitions parentStates) currentHands
        currentStates = zipWith3 State currentHands currentRatings transitionsFromParent
    in generateProgressions' currentStates others
    where transitions fromStates hand = map (flip transitionFromState hand) fromStates

generateProgressions' parentStates [] = GoalState $ map (flip (,) perfectRating) parentStates


