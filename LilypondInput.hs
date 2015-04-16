module LilypondInput where

import qualified Text.ParserCombinators.Parsec as P
import qualified Types as T
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import qualified Data.Map as Map

lilypondChordmode :: P.GenParser Char st [T.ChordSymbol]
lilypondChordmode = do
    chordmodeOpen
    chordSymbols <- P.sepBy chordSymbol P.space
    chordmodeClose
    return chordSymbols

chordmodeOpen = do
    P.spaces
    P.string "\\chordmode {"
    P.spaces

chordmodeClose = do
    P.spaces
    P.char '}'

chordSymbol :: P.GenParser Char st T.ChordSymbol
chordSymbol = do
    chordKey <- key
    duration
    mods <- modifiers
    let basic = T.ChordSymbol chordKey (Just T.Major) [] Nothing
    return $ applyMods basic mods

applyMods :: T.ChordSymbol -> [Modifier] -> T.ChordSymbol
applyMods cs [] = cs
applyMods cs (m:ms) = applyMods modifiedCurrent ms
    where modifiedCurrent = case m of
            NoTriadMod -> cs{T.chordMajMin=Nothing}
            MinorMod -> cs{T.chordMajMin=Just T.Minor}
            Tension t -> cs{T.chordTensions=(t : T.chordTensions cs)}

duration :: P.GenParser Char st String
duration = P.many P.alphaNum

key :: P.GenParser Char st T.PitchClass
key = do
    keyChar <- P.many1 P.lower
    return $ case Map.lookup keyChar mapping of
        Just pitchClass -> pitchClass
        Nothing -> error "invalid pitch class"
    where mapping = Map.fromList [
                ("ces", T.cFlat),
                ("c", T.C),
                ("cis", T.Csharp),
                ("des", T.dFlat),
                ("d", T.D),
                ("dis", T.Dsharp),
                ("ees", T.eFlat),
                ("es", T.eFlat),
                ("e", T.E),
                ("eis", T.eSharp),
                ("fes", T.fFlat),
                ("f", T.F),
                ("fis", T.Fsharp),
                ("ges", T.gFlat),
                ("g", T.G),
                ("gis", T.Gsharp),
                ("aes", T.aFlat),
                ("a", T.A),
                ("ais", T.Asharp),
                ("bes", T.bFlat),
                ("b", T.B),
                ("bis", T.bSharp)
            ]

modifiers :: P.GenParser Char st [Modifier]
modifiers = P.try $ do
    P.char ':'
    mods <- P.sepBy modifier (P.char '.')
    return . nub . concat $ mods
    P.<|> return []

modifier :: P.GenParser Char st [Modifier]
modifier = foldr f (g $ head modifierMapping) (tail modifierMapping) P.<?> "modifier"
    where f t accum = P.try (g t) P.<|> accum
          g (str, mods) = do
                P.string str
                return mods

-- This is based on http://lilypond.org/doc/v2.12/Documentation/user/lilypond/Common-chord-modifiers#Common-chord-modifiers
modifierMapping = sortBy (flip $ comparing $ length . fst) [
    ("", []),
    ("5", []),
    ("m", [MinorMod]),
    ("m5", [MinorMod]),
    ("aug", [Tension T.AugmentedFifth]),
    ("dim", [MinorMod, Tension T.DiminishedFifth]),
    ("7", [Tension T.Seventh]),
    ("maj7", [Tension T.MajorSeventh]),
    ("maj", [Tension T.MajorSeventh]),
    ("m7", [MinorMod, Tension T.Seventh]),
    ("dim7", [MinorMod, Tension T.DiminishedFifth, Tension T.DiminishedSeventh]),
    ("aug7", [Tension T.AugmentedFifth, Tension T.Seventh]),
    ("5-", [Tension T.DiminishedFifth]),
    ("6", [Tension T.Sixth]),
    ("m6", [MinorMod, Tension T.Sixth]),
    ("9", [Tension T.Seventh, Tension T.Ninth]),
    ("maj9", [Tension T.MajorSeventh, Tension T.Ninth]),
    ("m9", [MinorMod, Tension T.Seventh, Tension T.Ninth]),
    ("11", [Tension T.Seventh, Tension T.Ninth, Tension T.Eleventh]),
    ("maj11", [Tension T.MajorSeventh, Tension T.Ninth, Tension T.Eleventh]),
    ("m11", [MinorMod, Tension T.Seventh, Tension T.Ninth, Tension T.Eleventh]),
    ("13", [Tension T.Seventh, Tension T.Ninth, Tension T.Thirteenth]),
    ("maj13", [Tension T.MajorSeventh, Tension T.Ninth, Tension T.Eleventh, Tension T.Thirteenth]), -- This will be incorrect when it's combined with another modifier like "11", because it will have T.Seventh and T.MajorSeventh.
    ("m13", [MinorMod, Tension T.Seventh, Tension T.Ninth, Tension T.Thirteenth]),
    ("sus2", [NoTriadMod, Tension T.Ninth]),
    ("sus4", [NoTriadMod, Tension T.Eleventh])
    ]

data Modifier = Tension T.Tension
                 | MinorMod
                 | NoTriadMod
    deriving (Show, Eq)


parseLilypondChordmode :: String -> Either P.ParseError [T.ChordSymbol]
parseLilypondChordmode input = P.parse lilypondChordmode "(unknown)" input
