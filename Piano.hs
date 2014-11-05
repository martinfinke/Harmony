module Piano where

import Types

isBlackKey, isWhiteKey :: Pitch -> Bool
isBlackKey pitch = toPitchClass pitch `elem` [Csharp, Dsharp, Fsharp, Gsharp, Asharp]
isWhiteKey = not . isBlackKey


