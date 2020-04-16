# BDSCOD

## Running the examples

```
$ stack build
$ stack test
$ stack exec manceau-comparison
$ stack exec simulation-study
```

## Making the figures

```
$ Rscript R/llhd-profiles.R
$ Rscript R/manceau-comparison.R
```

This creates `out/llhd-profile-*.pdf` and `out/manceau-comparison.pdf`.
