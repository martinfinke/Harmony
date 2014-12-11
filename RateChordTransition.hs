{-|
Module      : RateChordTransition
Description : Functions to rate transitions between two 'Chord's (or 'Hand's).
-}
module RateChordTransition where

import Types
import Piano
import RateChord (Rating, perfectRating, standardPenalty)
import Data.Map as Map hiding (foldr)

-- | A function to rate the transition from one 'Hand' to another.
type ChordTransitionRater = Hand -> Hand -> Rating

-- | All 'ChordTransitionRater's in this module.
allChordTransitionRaters :: [ChordTransitionRater]
allChordTransitionRaters = [rate_avoidJumps 4,
                            rate_avoidJumpInHighestVoice 5,
                            rate_avoidTraversingBlackKeys,
                            rate_avoidHittingBetweenBlackKeys,
                            rate_avoidSpanDifference 7]

-- | Applies all 'ChordTransitionRater's, summing up the ratings from all of them.
totalTransitionRating :: (Hand, Hand) -> Rating
totalTransitionRating (hand1, hand2) = foldr (\current -> (+) (current hand1 hand2)) perfectRating allChordTransitionRaters

-- | Avoid jumps in any note.
rate_avoidJumps :: Semitones -- ^ Tolerance interval, jumps smaller or equal to this won't be penalized.
                -> ChordTransitionRater
rate_avoidJumps intervalTolerance hand1 hand2 =
    Map.fold (+) perfectRating penaltiesByFinger
    where penaltiesByFinger = Map.intersectionWith (rateInterval intervalTolerance) hand1 hand2

-- | Additional rule to avoid jumps in the highest voice, possibly with a different tolerance.
rate_avoidJumpInHighestVoice :: Semitones -- ^ Tolerance interval, jumps smaller or equal to this won't be penalized.
                             -> ChordTransitionRater
rate_avoidJumpInHighestVoice intervalTolerance hand1 hand2 =
    rateInterval intervalTolerance (highestNote hand1) (highestNote hand2)

-- | Utility function to rate whether an interval is too large, given a tolerance.
rateInterval :: Semitones -- ^ Tolerance value
             -> Semitones -- ^ Starting note
             -> Semitones -- ^ Target note
             -> Rating
rateInterval tolerance st1 st2 = fromIntegral . (max 0) . (subtract tolerance) $ absInterval st1 st2

-- | Penalize whenever a white key from the first chord has to traverse a black key to reach another white key. Only a problem when the hand is "into the keys".
rate_avoidTraversingBlackKeys :: ChordTransitionRater
rate_avoidTraversingBlackKeys from to
    | handIsIntoTheKeys from || handIsIntoTheKeys to =
        fromIntegral $ length traversingFingers
    | otherwise = perfectRating
    where maybeInnerRange p1 p2 = pitchRangeBetween (succ p1) (pred p2)
          traversesBlackKey p1 p2 = isWhiteKey p1 && case maybeInnerRange p1 p2 of
                Nothing -> False
                Just (lower, higher) -> any isBlackKey [lower..higher]
          traversingFingers = keys $ Map.intersectionWith traversesBlackKey from to

-- | Penalize whenever the 2nd, 3rd or 4th 'Finger' has to "hit" the white key between two black keys. This applies whenever it is not coming from an adjacent black key, and it isn't already on that key.
rate_avoidHittingBetweenBlackKeys :: ChordTransitionRater
rate_avoidHittingBetweenBlackKeys from to =
    fromIntegral . length $ hittingFingers
    where hitsBetween p1 p2 = p1 /= p2 && isWhiteKey p2 && (not $ areNeighbouring p1 p2) && all isBlackKey (neighbours p2)
          hittingFingers = keys $ Map.intersectionWith hitsBetween from to

-- | Avoid too high differences in span from one chord to the next.
-- The first parameter indicates how high the difference may be (in 'Semitones') without being penalized.
rate_avoidSpanDifference :: Semitones -> ChordTransitionRater
rate_avoidSpanDifference tolerance from to =
    (* penaltyPerSemitone) . fromIntegral . (max 0) . (subtract tolerance) $ spanDifference
    where spanDifference = semitoneSpan from - semitoneSpan to
          penaltyPerSemitone = 0.3

