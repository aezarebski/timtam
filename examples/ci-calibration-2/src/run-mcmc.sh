#!/usr/bin/env bash

NUM_SEEDS=$1                    # total number of replicates
PAR_SIZE=10                     # number of processes to run in parallel.

AGG_JSON=mcmc-app-config-aggregated.json
SEQ_JSON=mcmc-app-config.json

IX=1
OUTER_LOOP_LIMIT="$(($NUM_SEEDS-$PAR_SIZE+1))"
while [ $IX -le $OUTER_LOOP_LIMIT ]
do
    echo "-----"
    PIDS=()
    PAR_LIMIT="$(($IX+$PAR_SIZE))"
    while [ $IX -lt $PAR_LIMIT ]
    do
        PADDED_IX=$(printf "%03d" $IX)
        echo "Starting $IX"
        stack exec -- mcmc out/replicate-$IX/$SEQ_JSON && stack exec -- mcmc out/replicate-$IX/$AGG_JSON && echo "Finished $IX" & pids+=($!)
        ((IX++))
    done
    wait "${pids[@]}"
done
