# Openkolloq - Haskell

This document is meant to be compiled using pandoc.  

## Compiling

To compile, run the following command within the directory containing
`slides.md`

```
pandoc -t beamer --incremental --highlight-style kate --filter columnfilters.py --template="./custom.latex" --slide-level 3 slides.md -o slides.pdf
```

The filter enables writing columns like this:

```
[columns]
[column=0.5]
LEFT TEXT
[column=0.5]
RIGHT TEXT
[/columns]
```

It requires python and the `pandocfilters` package.