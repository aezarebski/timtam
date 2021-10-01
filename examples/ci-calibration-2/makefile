NUM_REPS=30
PARAMS_JSON=../example-parameters.json
BURNIN=0
NUM_MCMC_SAMPLES=2000
index : index.html

index.html : src/build-viewer.R out/prevalence-calibration.png
	Rscript src/build-viewer.R

out/prevalence-calibration.png : src/build-calibration-figures.R out/mcmc-summary.json
	Rscript src/build-calibration-figures.R --input out/mcmc-summary.json --output out/prevalence-calibration.png

out/mcmc-summary.json : out/mcmc-done.txt src/summarise-mcmc-samples.R
	Rscript src/summarise-mcmc-samples.R --num-replicates ${NUM_REPS} --parameters ${PARAMS_JSON} --burnin ${BURNIN} --output out/mcmc-summary.json

# This file is touched at the end of the MCMC as a way to keep track of when
# things have been run.
out/mcmc-done.txt : src/run-mcmc.sh out/simulation-done.txt
	/usr/bin/env bash ./src/run-mcmc.sh
	touch out/mcmc-done.txt

# This file is touched to indicate that the data have been simulated.
out/simulation-done.txt : src/simulate-datasets.R
	Rscript src/simulate-datasets.R --num-replicates ${NUM_REPS} --parameters ${PARAMS_JSON} --duration 50.0 --seq-agg-times="0 50.0 1" --occ-agg-times="0.5 50.0 7" --num-mcmc-samples ${NUM_MCMC_SAMPLES}
	touch out/simulation-done.txt

clean :
	rm -f index.html
	rm -f out/prevalence-calibration.png
	rm -f out/mcmc-done.txt
	rm -f out/simulation-done.txt
	rm -rf out/replicate-*
	rm -f mcmc-summary.json
	rm -f prevalence-calibration-extra-1.png
