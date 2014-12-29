{-|
Module      : RateSubProgression
Description : Functions to rate progressions of one or more 'Chord's (or 'Hand's).
-}
module RateSubProgression where

import Types
import Piano
import Data.Map as Map hiding (foldr)

-- | A function to rate the transitions between 'Hand's.
type SubProgressionRater = [Hand] -- ^ The 'Hand' transition in reverse order, i.e. the head of this list is the last 'Hand' in the transition.
                         -> Rating

-- | All 'SubProgressionRater's in this module.
allSubProgressionRaters :: [SubProgressionRater]
allSubProgressionRaters = [rate_avoidJumps (Semitones 4),
                            rate_avoidJumpInHighestVoice (Semitones 5),
                            rate_avoidTraversingBlackKeys,
                            rate_avoidHittingBetweenBlackKeys,
                            rate_avoidSpanDifference (Semitones 7),
                            rate_avoidClustering,
                            rate_avoidSpreading
                            ]

-- | Applies all 'SubProgressionRater's, summing up the ratings from all of them.
totalRating :: [Hand] -> Rating
totalRating hands = foldr ((+) . ($ hands)) perfectRating allSubProgressionRaters

-- | Avoid jumps in any note.
rate_avoidJumps :: Semitones -- ^ Tolerance interval, jumps smaller or equal to this won't be penalized.
                -> SubProgressionRater
rate_avoidJumps intervalTolerance (hand2:hand1:_) =
    Map.fold (+) perfectRating penaltiesByFinger
    where penaltiesByFinger = Map.intersectionWith (rateInterval intervalTolerance) hand1 hand2
rate_avoidJumps _ _ = perfectRating

-- | Additional rule to avoid jumps in the highest voice, possibly with a different tolerance.
rate_avoidJumpInHighestVoice :: Semitones -- ^ Tolerance interval, jumps smaller or equal to this won't be penalized.
                             -> SubProgressionRater
rate_avoidJumpInHighestVoice intervalTolerance (hand2:hand1:_) =
    rateInterval intervalTolerance (highestNote hand1) (highestNote hand2)
rate_avoidJumpInHighestVoice _ _ = perfectRating

-- | Utility function to rate whether an interval is too large, given a tolerance.
rateInterval :: Semitones -- ^ Tolerance value
             -> Pitch -- ^ Starting note
             -> Pitch -- ^ Target note
             -> Rating
rateInterval (Semitones tolerance) pitch1 pitch2 = fromIntegral . (max 0) . (subtract tolerance) $ interval
    where (Semitones interval) = absInterval pitch1 pitch2

-- | Penalize whenever a white key from the first chord has to traverse a black key to reach another white key. Only a problem when the hand is "into the keys".
rate_avoidTraversingBlackKeys :: SubProgressionRater
rate_avoidTraversingBlackKeys (hand2:hand1:_)
    | handIsIntoTheKeys hand1 || handIsIntoTheKeys hand2 =
        fromIntegral $ length traversingFingers
    | otherwise = perfectRating
    where maybeInnerRange p1 p2 = pitchRangeBetween (succ p1) (pred p2)
          traversesBlackKey p1 p2 = isWhiteKey p1 && case maybeInnerRange p1 p2 of
                Nothing -> False
                Just (lower, higher) -> any isBlackKey [lower..higher]
          traversingFingers = keys $ Map.intersectionWith traversesBlackKey hand1 hand2
rate_avoidTraversingBlackKeys _ = perfectRating

-- | Penalize whenever the 2nd, 3rd or 4th 'Finger' has to "hit" the white key between two black keys. This applies whenever it is not coming from an adjacent black key, and it isn't already on that key.
rate_avoidHittingBetweenBlackKeys :: SubProgressionRater
rate_avoidHittingBetweenBlackKeys (hand2:hand1:_) =
    fromIntegral . length $ hittingFingers
    where hitsBetween p1 p2 = p1 /= p2 && isWhiteKey p2 && (not $ areNeighbouring p1 p2) && all isBlackKey (neighbours p2)
          hittingFingers = keys $ Map.intersectionWith hitsBetween hand1 hand2
rate_avoidHittingBetweenBlackKeys _ = perfectRating

-- | Avoid too high differences in span from one chord to the next.
-- The first parameter indicates how high the difference may be (in 'Semitones') without being penalized.
rate_avoidSpanDifference :: Semitones -> SubProgressionRater
rate_avoidSpanDifference (Semitones tolerance) (hand2:hand1:_) =
    (* penaltyPerSemitone) . fromIntegral . (max 0) . (subtract tolerance) $ spanDifference
    where spanDifference = span1 - span2
          (Semitones span1) = semitoneSpan hand1
          (Semitones span2) = semitoneSpan hand2
          penaltyPerSemitone = 0.3
rate_avoidSpanDifference _ _ = perfectRating








-- | No two adjacent intervals in a chord should be a total of 4 semitones or less.
rate_avoidClustering :: SubProgressionRater
rate_avoidClustering = rate . toChord . head
    where rate chord = case chord of
              Chord (st:st':st'':sts) -> rateSingle (absInterval st st'') + (rate $ Chord (st':st'':sts))
              _ -> perfectRating
          rateSingle interval = if interval > minSpreadSemitones
                                    then perfectRating
                                    else standardPenalty
          minSpreadSemitones = Semitones 4

-- | Chords that contain intervals higher than a quint (7 semitones) tend to sound "torn apart".
rate_avoidSpreading :: SubProgressionRater
rate_avoidSpreading = rate . toChord . head
    where rate chord = case chord of
              Chord (st:st':sts) -> rateSingle (absInterval st st') + (rate $ Chord (st':sts))
              _ -> perfectRating
          rateSingle interval = if interval > maxSpreadSemitones
                                    then standardPenalty
                                    else perfectRating
          maxSpreadSemitones = Semitones 7

-- | Penalize whenever the lowest finger may accidentally hit a black key (above the white key it is playing), or when the highest finger may accidentally hit a black key (below the white key it is playing).
-- This only applies when the hand is "into the keys", and is never a problem for small chord spans.
-- TODO: Should be changed so that safeDistance is a ratio, and is checked for each Finger-Finger interval
rate_avoidAccidentalBlackKeyHit :: Distance -> SubProgressionRater
rate_avoidAccidentalBlackKeyHit safeDistance (hand:_)
    | handSpan hand > safeDistance && handIsIntoTheKeys hand = penaltyForLowest + penaltyForHighest
    | otherwise = perfectRating
    where lowest = lowestNote hand
          lowestIsBad = isWhiteKey lowest && isBlackKey (succ lowest)
          penaltyForLowest = if lowestIsBad then standardPenalty else perfectRating
          highest = highestNote hand
          highestIsBad = isWhiteKey highest && isBlackKey (pred highest)
          penaltyForHighest = if highestIsBad then standardPenalty else perfectRating





