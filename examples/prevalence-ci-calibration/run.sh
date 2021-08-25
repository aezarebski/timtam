#!/usr/bin/env bash
#
# run.sh
#
# This must be run with bash!!!

NUM_SEEDS=100
VIS_JSON=out/vis-data.json

Rscript src/generate-config-files.R 1 $NUM_SEEDS

for ix in 00 10 20 30 40 50 60 70 80 90;
do
    pids=()

    for jx in {1..10};
    do
        kx=$(($ix + $jx))
        stack exec -- simulation-study-aggregated-observations out/seed-$kx/config-$kx.json && echo "Finished $kx" & pids+=($!)
    done

    wait "${pids[@]}"
done

Rscript src/summarize-results.R $NUM_SEEDS $VIS_JSON
Rscript src/plotter.R $VIS_JSON
