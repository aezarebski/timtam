# BDSCOD

This package defines the approximate likelihood of a birth-death process with
various observation processes.

## Running the Manceau example

```
$ stack build
$ stack test
$ stack exec manceau-comparison
$ Rscript R/manceau-comparison.R
```

This creates `out/manceau-comparison.pdf`.

## Simulation study

See `examples/simulation-study/README.md` for details of the simulation study.
