module RateChord where

import Types
import Piano

-- | A 'Rating' is always >= 0, and a low value is better.
type Rating = Float

-- | A function to rate a 'Hand'.
type ChordRater = Hand -> Rating

perfectRating, standardPenalty :: Rating
-- | The rating given to a flawless hand.
perfectRating = 0.0
-- | The penalty added to a hand whenever it violates a rule.
standardPenalty = 1.0

allChordRaters :: [ChordRater]
allChordRaters = [rate_avoidClustering,
                  rate_avoidSpreading]

-- | Applies all 'ChordRater's, summing up the ratings from all of them.
totalRating :: Hand -> Rating
totalRating hand = foldr ((+) . ($ hand)) perfectRating allChordRaters

-- | No two adjacent intervals in a chord should be a total of 4 semitones or less.
rate_avoidClustering :: ChordRater
rate_avoidClustering = rate . toChord
    where rate chord = case chord of
              Chord (st:st':st'':sts) -> rateSingle (absInterval st st'') + (rate $ Chord (st':st'':sts))
              _ -> perfectRating
          rateSingle interval = if interval > minSpreadSemitones
                                    then perfectRating
                                    else standardPenalty
          minSpreadSemitones = 4

-- | Chords that contain intervals higher than a quint (7 semitones) tend to sound "torn apart".
rate_avoidSpreading :: ChordRater
rate_avoidSpreading = rate . toChord
    where rate chord = case chord of
              Chord (st:st':sts) -> rateSingle (absInterval st st') + (rate $ Chord (st':sts))
              _ -> perfectRating
          rateSingle interval = if interval > maxSpreadSemitones
                                    then standardPenalty
                                    else perfectRating
          maxSpreadSemitones = 7

-- | Penalize whenever the lowest finger may accidentally hit a black key (above the white key it is playing), or when the highest finger may accidentally hit a black key (below the white key it is playing).
-- This only applies when the hand is "into the keys", and is never a problem for small chord spans.
rate_avoidAccidentalBlackKeyHit :: Distance -> ChordRater
rate_avoidAccidentalBlackKeyHit safeDistance hand
    | handSpan hand > safeDistance && handIsIntoTheKeys hand = penaltyForLowest + penaltyForHighest
    | otherwise = perfectRating
    where lowest = lowestNote hand
          lowestIsBad = isWhiteKey lowest && isBlackKey (succ lowest)
          penaltyForLowest = if lowestIsBad then standardPenalty else perfectRating
          highest = highestNote hand
          highestIsBad = isWhiteKey highest && isBlackKey (pred highest)
          penaltyForHighest = if highestIsBad then standardPenalty else perfectRating

-- | Whenever two fingers are spread far apart (i.e. near their maximum allowed spread), and the hand is "into the keys", 



