# Curry Time - Learn You a Haskell

## Intro

Slides from the talk _Curry-Time - Learn You a Haskell_ held by Cameron Reuschel
and Vincent Truchseß in a series of _Open Kolloq_ talks at the _Department of
Computer Science - University of Würzburg_.


## Compiling the Slides

### Dependencies

- pandoc
- pandocfilters
- python3

### Building PDF Slides

To compile, run the following command within the directory containing
`slides.md`. 

```
pandoc -t beamer --filter columnfilters.py  --slide-level 3 slides.md -o slides.pdf
```

This will create the file `slides.pdf` inside the working directory.  
