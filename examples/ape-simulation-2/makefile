SEED=4
DURATION=40.0
PARAM_JSON=../example-parameters.json
OUTPUT_DIR=out
APE_SIM=../ape-simulation/ape-sim.R

index: index.html

# 1. Run the simulation to generate the raw tree data.
out/ape-simulation-figure.png out/ape-sim-event-times.csv out/ape-sim-final-prevalence.json &: ${APE_SIM} ${PARAM_JSON}
	${APE_SIM} --seed ${SEED} -p ${PARAM_JSON} -o ${OUTPUT_DIR} --duration ${DURATION} --make-plots -v

# 2. Post-process the simulation data so it can be used in an MCMC sampler. This
# needs to be done once for each type of analysis that we want to run.
out/unscheduled-data/mcmc-app-config.json : out/ape-sim-event-times.csv src/make-config.R
	Rscript src/make-config.R --input out/ape-sim-event-times.csv --output out/unscheduled-data/mcmc-app-config.json --unscheduled-data

# 3. Run the MCMC using each of the data sets derived from the ape simulation.
out/unscheduled-data/posterior-samples.csv : out/unscheduled-data/mcmc-app-config.json
	stack exec -- mcmc out/unscheduled-data/mcmc-app-config.json

# 4. Run the diagnostics on the MCMC samples to check they are acceptable.
# out/unscheduled-data/mcmc-diagnostics.txt : out/unscheduled-data/posterior-samples.csv src/run-diagnostics.R
# 	Rscript src/run-diagnostics.R --input out/unscheduled-data/posterior-samples.csv

# 5. Visualise the results so that we have figure files handy with the results.
out/unscheduled-data/marginal-distributions.png : out/unscheduled-data/posterior-samples.csv src/post-processing-unscheduled-data.R out/ape-sim-final-prevalence.json
	Rscript src/post-processing-unscheduled-data.R --verbose --output-directory out/unscheduled-data --prevalence out/ape-sim-final-prevalence.json --posterior-samples out/unscheduled-data/posterior-samples.csv

# 6. Combine the results into a page for easy inspection.
index.html : out/ape-simulation-figure.png out/unscheduled-data/marginal-distributions.png src/make-viewer.R
	Rscript src/make-viewer.R

clean:
	rm -f out/ape-simulation-figure.png
	rm -f out/ape-sim-event-times.csv
	rm -f out/ape-sim-final-prevalence.json
	rm -f out/unscheduled-data/mcmc-app-config.json
	rm -f out/unscheduled-data/mcmc-samples.csv
	rm -f out/unscheduled-data/posterior-samples.csv
	rm -f out/unscheduled-data/traceplot-1.png
	rm -f out/unscheduled-data/traceplot-2.png
	rm -f out/unscheduled-data/traceplot-3.png
