#!/usr/bin/env bash

SEED=1

./ape-sim.R --seed $SEED -p ../example-parameters.json -o out --duration 25.0 --make-plots -v --rho 0.6

Rscript src/prep-mcmc-data.R

stack exec -- mcmc out/simulation-data.json

Rscript src/mcmc-plots.R

Rscript src/make-viewer.R
