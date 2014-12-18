{-|
Module      : GenerateChords
Description : Generates all possible 'Hand' progressions for a given 'ChordSymbol' progression.

The result is a 'State' graph that can be used by the 'Optimize' module to find the optimal progression.
-}
module GenerateChords where

import Types
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import RateChord
import RateChordTransition

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


-- | Recursive data structure to describe the state graph.
-- Nodes in the graph are 'State's, each corresponding to one possible way to play a 'ChordSymbol' (i.e. a 'Hand'), along with a 'Rating' given by the 'RateChord' module.
-- Every graph contains exactly one 'InitialState', which is a sentinel value for the beginning of the song.
-- Every graph contains exactly one 'GoalState', a sentinel for the end of the song.
-- Each 'State' except for the 'InitialState' may be connected with zero or more parent states. *Parent* means that the chord is played right before the current one, so the "connection" is a transition from one chord to another. It is a pair of the parent 'State' and a 'Rating' from the 'RateChordTransition' module. The 'Rating' describes the quality of transitioning from the parent's 'Hand' to the current one.
data State = InitialState
          | GoalState [(State, Rating)]
          | State Hand Rating [(State, Rating)]
    deriving(Show)

-- | Generates a graph of all possible 'Hand' progressions for a list of 'ChordSymbol's
-- Returns a 'GoalState' that can be used by the 'Optimize' module to find the optimal chord progression.
generateProgressions :: [ChordSymbol] -> State
generateProgressions = generateProgressions' [InitialState]

-- | Algorithm to generate a 'State' graph with all possible progressions.
-- It starts from the beginning of the song ('InitialState') and moves, one 'ChordSymbol' at a time, until there are no more 'ChordSymbol's, and the 'GoalState' is generated.
generateProgressions' :: [State] -- ^ All parent states (i.e. all possible 'Hand's for the previous 'ChordSymbol')
                      -> [ChordSymbol] -- ^ The remaining 'ChordSymbol's to generate 'State's for
                      -> State -- ^ Always returns a 'GoalState'.
generateProgressions' parentStates (currentSymbol:others) =
    let 
        -- All possible 'Hand's for the current 'ChordSymbol':
        currentHands = handsForChordSymbol currentSymbol
        -- Along with their 'ChordRating's:
        currentRatings = map totalRating currentHands
        -- All possible transitions (from parent 'State's to the current one):
        transitionsFromParent = map (transitionFromStates parentStates) currentHands
        -- All possible 'State's for the current 'ChordSymbol':
        currentStates = zipWith3 State currentHands currentRatings transitionsFromParent
    -- Recurse, going from the current 'ChordSymbol' to the next:
    in generateProgressions' currentStates others

generateProgressions' parentStates [] = GoalState $ map (flip (,) perfectRating) parentStates

-- | Create (and rate) the transition from a parent 'State' to a 'Hand' of the current 'State'.
transitionFromState :: State -> Hand -> (State, Rating)
transitionFromState InitialState _ = (InitialState, perfectRating)
transitionFromState (GoalState _) _ = error "Trying to transition out of GoalState!"
transitionFromState state@(State parentHand _ _) hand =
    (state, totalTransitionRating (parentHand, hand))

-- | Create (and rate) all transitions from a list of 'States' to a 'Hand'
transitionFromStates :: [State] -> Hand -> [(State, Rating)]
transitionFromStates fromStates hand = map (flip transitionFromState hand) fromStates
