#!/usr/bin/env bash

SEED=2
PARAM_JSON=../example-parameters.json

./ape-sim.R --seed $SEED -p $PARAM_JSON -o out --duration 25.0 --make-plots -v

Rscript src/prep-mcmc-data.R

stack exec -- mcmc out/simulation-data.json

Rscript src/mcmc-plots.R

Rscript src/make-viewer.R
