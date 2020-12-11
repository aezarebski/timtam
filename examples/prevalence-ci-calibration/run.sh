

Rscript scratch.R

stack exec -- simulation-study-aggregated-observations out/seed-1/config-1.json
stack exec -- simulation-study-aggregated-observations out/seed-2/config-2.json
stack exec -- simulation-study-aggregated-observations out/seed-3/config-3.json
stack exec -- simulation-study-aggregated-observations out/seed-4/config-4.json
stack exec -- simulation-study-aggregated-observations out/seed-5/config-5.json
stack exec -- simulation-study-aggregated-observations out/seed-6/config-6.json
stack exec -- simulation-study-aggregated-observations out/seed-7/config-7.json
stack exec -- simulation-study-aggregated-observations out/seed-8/config-8.json
stack exec -- simulation-study-aggregated-observations out/seed-9/config-9.json
stack exec -- simulation-study-aggregated-observations out/seed-10/config-10.json

Rscript plotter.R

cat out/seed-1/summary-seed-1.csv
