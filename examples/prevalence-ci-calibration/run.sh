

Rscript scratch.R

stack exec -- simulation-study-aggregated-observations out/seed-1/config-1.json

Rscript plotter.R

cat out/seed-1/summary-seed-1.csv
