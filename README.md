# Harmony
An algorithm to generate optimal chord arrangements for chord symbols.

## Requirements
- [Haskell](https://www.haskell.org/platform/)
- [cabal](https://www.haskell.org/cabal/) `>= 1.18` (for sandbox support)
- [LilyPond](http://www.lilypond.org/) for PDF output

Other requirements (like [FGL](https://hackage.haskell.org/package/fgl)) will be automatically installed into the sandbox through `cabal`.

## Installation
```bash
git clone https://github.com/martinfinke/Harmony.git
cd Harmony
cabal sandbox init
cabal install --only-dependencies

```

## Usage
```bash
cabal exec ghci Main
```


```haskell
> -- Render the optimal chord arrangement of each example song to a PDF file:
> allExamplesToPdf
> -- The PDF files are saved in the Harmony root directory.

> -- View the chord symbols of an example song:
> giantSteps
[B maj7,D 7, …

> -- Calculate an optimal chord arrangement for a song:
> optimalHandProgression giantSteps
[fromList [(2,Pitch 58),(3,Pitch 54),(4,Pitch 51),(5,Pitch 47)],…

> -- Calculate the total cost for a chord arrangement:
> minimumCost giantSteps
111.0

> -- Get the raw LilyPond data of an optimal chord arrangement:
> handProgressionToLilypondString $ optimalHandProgression giantSteps
"{   <b'[] dis''[] fis''[] ais''[]> … \n}"
```

## Haddock Documentation
Run `cabal haddock` and open `dist/doc/html/Harmony/index.html` in your favourite browser.


