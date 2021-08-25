#!/usr/bin/env bash
#
# run.sh
#
# This must be run with bash!!!

NUM_SEEDS=100                   # total number of replicates
PAR_SIZE=15                     # number of processes to run in parallel.
VIS_JSON=out/vis-data.json

# the arguments specify the lower and upper limit on the range of seeds to use.
Rscript src/generate-config-files.R 1 $NUM_SEEDS

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
        stack exec -- simulation-study-aggregated-observations out/seed-$PADDED_IX/config-$PADDED_IX.json && echo "Finished $IX" & pids+=($!)
        ((IX++))
    done
    wait "${pids[@]}"
done


Rscript src/summarize-results.R $NUM_SEEDS $VIS_JSON
Rscript src/plotter.R $VIS_JSON
