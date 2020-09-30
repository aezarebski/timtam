rm out/*
rm ts-config.json

stack build

Rscript src/make-json-config.R

stack exec -- simulation-study-time-series ts-config.json

Rscript src/plot-simulated-data.R
Rscript src/plot-llhd-profiles.R
