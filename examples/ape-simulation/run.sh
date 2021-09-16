#!/usr/bin/env bash

SEED=3
DURATION=40.0
PARAM_JSON=../example-parameters.json

# Without the rho sample at the end...
./ape-sim.R --seed $SEED -p $PARAM_JSON -o out --duration $DURATION --make-plots -v

# With the rho sample at the end...
# ./ape-sim.R --seed $SEED -p $PARAM_JSON -o out --duration $DURATION --make-plots -v --rho 0.6

Rscript src/prep-mcmc-data.R

stack exec -- mcmc out/simulation-data.json

Rscript src/mcmc-plots.R

Rscript src/make-viewer.R
