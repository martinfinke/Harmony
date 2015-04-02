# Harmony
An algorithm to generate optimal chord arrangements for chord symbols.

For more details, please refer to the [paper](http://martin-finke.de/documents/Masterprojekt_Harmony_Finke.pdf) (German, with english abstract). To use the Harmony version described in the paper, check out the corresponding commit (after cloning):

```bash
git checkout 427ec7cb95e64f2ba40c58e5e1e2a36f556cdd36
```

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

For more specific information about included functionality, please refer to the Haddock documentation.

## Haddock Documentation
Run `cabal haddock` and open `dist/doc/html/Harmony/index.html` in your favourite browser.

## License
The MIT License (MIT)

Copyright (c) <year> <copyright holders>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
