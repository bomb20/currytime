# Openkolloq - Haskell

This document is ment to be compiled using pandoc.  

## Compiling

To compile, run the following command within the directory containing
`slides.md`

```
pandoc -t beamer --incremental --template="./custom.latex" --slide-level 3
slides.md -o slides.pdf
```

