
index : index.html

index.html : src/build-viewer.R out/prevalence-calibration.png
	Rscript src/build-viewer.R

out/prevalence-calibration.png : src/build-prevalence-calibration.R out/mcmc-done.txt
	Rscript src/build-prevalence-calibration.R

# This file is touched at the end of the MCMC as a way to keep track of when
# things have been run.
out/mcmc-done.txt : src/run-mcmc.sh out/simulation-done.txt
	/usr/bin/env bash ./src/run-mcmc.sh
	touch out/mcmc-done.txt

# This file is touched to indicate that the data have been simulated.
out/simulation-done.txt : src/simulate-datasets.R
	Rscript src/simulate-datasets.R
	touch out/simulation-done.txt

clean :
	rm -f index.html
	rm -f out/prevalence-calibration.png
	rm -f out/mcmc-done.txt
	rm -f out/simulation-done.txt
	rm -rf out/replicate-*
