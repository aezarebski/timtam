#!/usr/bin/env bash

SEED=5

./ape-sim.R --seed $SEED -p ../example-parameters.json -o out --duration 40.0 --make-plots -v

Rscript src/prep-mcmc-data.R

stack exec -- mcmc out/simulation-data.json

Rscript src/mcmc-plots.R

Rscript src/make-viewer.R
