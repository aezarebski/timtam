#!/usr/bin/env bash

NUM_SEEDS=100                   # total number of replicates
PAR_SIZE=15                     # number of processes to run in parallel.

IX=1
OUTER_LOOP_LIMIT="$(($NUM_SEEDS-$PAR_SIZE))"
while [ $IX -le $OUTER_LOOP_LIMIT ]
do
    PIDS=()
    PAR_LIMIT="$(($IX+$PAR_SIZE))"
    while [ $IX -le $PAR_LIMIT ]
    do
        PADDED_IX=$(printf "%03d" $IX)
        echo "Starting $IX"
        mkdir out/replicate-$IX && touch out/replicate-$IX/foobar.txt && sleep 1 && echo "Finished $IX" & pids+=($!)
        ((IX++))
    done
    wait "${pids[@]}"
done
