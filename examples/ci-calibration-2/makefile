NUM_REPS=100
PARAMS_JSON=../example-parameters.json
DUR=50.0
BURNIN=500
NUM_MCMC_SAMPLES=5500

index : index.html

index.html : src/build-viewer.R out/plotting-done.txt
	Rscript src/build-viewer.R

out/plotting-done.txt : src/build-calibration-figures.R out/mcmc-summary.json
	Rscript src/build-calibration-figures.R --input out/mcmc-summary.json --output out/prevalence-calibration.png
	touch out/plotting-done.txt

out/mcmc-summary.json : out/mcmc-done.txt src/summarise-mcmc-samples.R
	Rscript src/summarise-mcmc-samples.R --num-replicates ${NUM_REPS} --parameters ${PARAMS_JSON} --burnin ${BURNIN} --output out/mcmc-summary.json

# This file is touched at the end of the MCMC as a way to keep track of when
# things have been run.
out/mcmc-done.txt : src/run-mcmc.sh out/simulation-done.txt
	/usr/bin/env bash ./src/run-mcmc.sh ${NUM_REPS}
	touch out/mcmc-done.txt

# This file is touched to indicate that the data have been simulated.
out/simulation-done.txt : src/simulate-datasets.R
	Rscript src/simulate-datasets.R --num-replicates ${NUM_REPS} --parameters ${PARAMS_JSON} --duration ${DUR} --seq-agg-times="0 ${DUR} 1" --occ-agg-times="0.5 ${DUR} 7" --num-mcmc-samples ${NUM_MCMC_SAMPLES}
	touch out/simulation-done.txt

clean :
	rm -f index.html
	rm -f out/prevalence-calibration.png
	rm -f out/mcmc-done.txt
	rm -f out/simulation-done.txt
	rm -rf out/replicate-*
	rm -f out/mcmc-summary.json
	rm -f out/prevalence-calibration-extra-1.png
	rm -f out/prevalence-calibration-extra-2.png
	rm -f out/aggregated-estimate-calibration.png
	rm -f out/estimate-calibration.png
	rm -f out/effective-sample-sizes.png
	rm -f out/plotting-done.txt
