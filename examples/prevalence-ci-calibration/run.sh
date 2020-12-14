
NUM_SEEDS=3

Rscript scratch.R $NUM_SEEDS

for ix in `seq 1 $NUM_SEEDS`;
do
    stack exec -- simulation-study-aggregated-observations out/seed-$ix/config-$ix.json
done

Rscript plotter.R $NUM_SEEDS
