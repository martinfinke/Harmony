module LilypondOutput where

import Types as T
import Music.Lilypond as L
import Text.Pretty(pretty)
import Data.Map as Map hiding (map)
import System.Process


pitchToLilypond :: T.Pitch -> L.Pitch
pitchToLilypond p = L.Pitch (pitchName, accidental, o)
    where (pitchClass, Octave o) = T.fromPitch p
          (pitchName, accidental) = pitchClassToLilypond pitchClass

pitchClassToLilypond :: T.PitchClass -> (L.PitchName, L.Accidental)
pitchClassToLilypond pc = case pc of
    T.C -> (L.C, 0)
    T.Csharp -> (L.C, 1)
    T.D -> (L.D, 0)
    T.Dsharp -> (L.D, 1)
    T.E -> (L.E, 0)
    T.F -> (L.F, 0)
    T.Fsharp -> (L.F, 1)
    T.G -> (L.G, 0)
    T.Gsharp -> (L.G, 1)
    T.A -> (L.A, 0)
    T.Asharp -> (L.A, 1)
    T.B -> (L.B, 0)

handToLilypond :: T.Hand -> L.Music
handToLilypond h = L.Chord withEmptyPostEvents Nothing []
    where T.Chord pitches = T.toChord h
          lilypondPitches = map pitchToLilypond pitches
          lilypondNotes = map (flip L.NotePitch Nothing) lilypondPitches
          withEmptyPostEvents = zip lilypondNotes (repeat [])

handProgressionToLilypondExpression :: [T.Hand] -> L.Music
handProgressionToLilypondExpression = L.Sequential . map handToLilypond

handProgressionToLilypondString :: [T.Hand] -> String
handProgressionToLilypondString = show . pretty . handProgressionToLilypondExpression

handProgressionToPdf :: String -> [T.Hand] -> IO ()
handProgressionToPdf fileName hands = do
    lilypondStdout <- readProcess "lilypond" ["-o", fileName, "-"] (handProgressionToLilypondString hands)
    putStrLn ("Output from Lilypond: " ++ lilypondStdout)
