module RateChord where

import Types

type ChordRater = Chord -> Float

perfectRating, standardPenalty :: Float
perfectRating = 0.0
standardPenalty = 1.0

-- | No two adjacent intervals in a chord should be a total of 4 semitones or less.
-- | Assumes that the chord is sorted (i.e. lowest note first).
rate_avoidClustering :: ChordRater
rate_avoidClustering (Chord semitones) = case semitones of
    st:st':st'':sts -> rateSingle (st''-st) + (rate_avoidClustering $ Chord (st':st'':sts))
    _ -> perfectRating
    where rateSingle interval = if abs interval > minSpreadSemitones
                                    then perfectRating
                                    else clusterPenalty
          minSpreadSemitones = 4
          clusterPenalty = standardPenalty
          

-- | Chords that contain intervals higher than a quint (7 semitones) tend to sound "torn apart".
rate_avoidSpreading :: ChordRater
rate_avoidSpreading (Chord semitones) = case semitones of
    st:st':sts -> rateSingle (st'-st) + (rate_avoidSpreading $ Chord (st':sts))
    _ -> perfectRating
    where rateSingle interval = if abs interval > maxSpreadSemitones
                                    then spreadPenalty
                                    else perfectRating
          maxSpreadSemitones = 7
          spreadPenalty = standardPenalty



