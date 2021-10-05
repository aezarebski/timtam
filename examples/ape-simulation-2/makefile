SEED=1
DURATION=50.0
PARAM_JSON=../example-parameters.json
OUTPUT_DIR=out
APE_SIM=../ape-simulation/ape-sim.R
UNSCHEDULED_PARAM="identity-muKnown-lambda-psi-noRho-omega-noNu"
AGGREGATED_PARAM="identity-muKnown-lambda-psiZero-rho-omegaZero-nu"
AGG_DUR_SEQ=1
AGG_DUR_OCC=7
AGGREGATION_ARGS=--seq-agg-times="0 ${DURATION} ${AGG_DUR_SEQ}" --occ-agg-times="0.5 ${DURATION} ${AGG_DUR_OCC}"
PREV_JSON=out/ape-sim-final-prevalence.json

index: index.html

tmp: out/aggregated-data/mcmc-app-config.json

# 1. Run the simulation to generate the raw tree data.
out/ape-simulation-figure.png out/ape-simulation-figure-aggregated.png out/ape-sim-aggregated-event-times.csv out/ape-sim-event-times.csv ${PREV_JSON} &: ${APE_SIM} ${PARAM_JSON}
	${APE_SIM} --seed ${SEED} -p ${PARAM_JSON} -o ${OUTPUT_DIR} --duration ${DURATION} --make-plots -v ${AGGREGATION_ARGS}

# 2. Post-process the simulation data so it can be used in an MCMC sampler. This
# needs to be done once for each type of analysis that we want to run.
out/unscheduled-data/mcmc-app-config.json : out/ape-sim-event-times.csv src/make-config.R
	Rscript src/make-config.R --input out/ape-sim-event-times.csv --output out/unscheduled-data/mcmc-app-config.json --parameterisation=${UNSCHEDULED_PARAM} --unscheduled-data --num-mcmc-samples 500000

out/aggregated-data/mcmc-app-config.json : out/ape-sim-aggregated-event-times.csv src/make-config.R
	Rscript src/make-config.R --input out/ape-sim-aggregated-event-times.csv --output out/aggregated-data/mcmc-app-config.json --parameterisation=${AGGREGATED_PARAM} --num-mcmc-samples 2000000

# 3. Run the MCMC using each of the data sets derived from the ape simulation.
out/unscheduled-data/posterior-samples.csv : out/unscheduled-data/mcmc-app-config.json
	stack exec -- mcmc out/unscheduled-data/mcmc-app-config.json

out/aggregated-data/posterior-samples.csv : out/aggregated-data/mcmc-app-config.json
	stack exec -- mcmc out/aggregated-data/mcmc-app-config.json

# 4. Visualise the results so that we have figure files handy with the results.
out/unscheduled-data/unscheduled-figures-dummy.txt : out/unscheduled-data/posterior-samples.csv \
                                                     src/post-mcmc-visualisation.R
	Rscript src/post-mcmc-visualisation.R --verbose --output-directory out/unscheduled-data --prevalence ${PREV_JSON} --posterior-samples out/unscheduled-data/posterior-samples.csv --params="llhd,birth_rate,sampling_rate,omega_rate,nb_r,nb_p" --burn 10000
	touch out/unscheduled-data/unscheduled-figures-dummy.txt

out/aggregated-data/aggregated-figures-dummy.txt : out/aggregated-data/posterior-samples.csv \
                                                                                                                      src/post-mcmc-visualisation.R
	Rscript src/post-mcmc-visualisation.R --verbose --output-directory out/aggregated-data --prevalence ${PREV_JSON} --posterior-samples out/aggregated-data/posterior-samples.csv --params="llhd,birth_rate,rho_prob,nu_prob,nb_r,nb_p" --burn 10000
	touch out/aggregated-data/aggregated-figures-dummy.txt

# 5. Combine the results into a page for easy inspection.
index.html : out/ape-simulation-figure.png \
             src/assemble-plots.R \
             out/unscheduled-data/unscheduled-figures-dummy.txt \
             out/aggregated-data/aggregated-figures-dummy.txt \
             src/make-viewer.R
	Rscript src/assemble-plots.R
	Rscript src/make-viewer.R

clean:
	rm -f out/ape-simulation-figure.png
	rm -f out/ape-simulation-figure-aggregated.png
	rm -f out/ape-sim-aggregated-event-times.csv
	rm -f out/ape-sim-event-times.csv
	rm -f out/ape-sim-final-prevalence.json
	rm -f out/unscheduled-data/mcmc-app-config.json
	rm -f out/unscheduled-data/mcmc-samples.csv
	rm -f out/unscheduled-data/posterior-samples.csv
	rm -f out/unscheduled-data/traceplot-1.png
	rm -f out/unscheduled-data/traceplot-2.png
	rm -f out/unscheduled-data/traceplot-3.png
	rm -f out/unscheduled-data/marginal-distributions.png
	rm -f out/unscheduled-data/prevalence.png
	rm -f out/unscheduled-data/splom.png
	rm -f out/unscheduled-data/r-naught.png
	rm -f out/unscheduled-data/marginal-distributions-2.rds
	rm -f out/aggregated-data/mcmc-app-config.json
	rm -f out/aggregated-data/mcmc-samples.csv
	rm -f out/aggregated-data/posterior-samples.csv
	rm -f out/aggregated-data/traceplot-1.png
	rm -f out/aggregated-data/traceplot-2.png
	rm -f out/aggregated-data/traceplot-3.png
	rm -f out/aggregated-data/marginal-distributions.png
	rm -f out/aggregated-data/prevalence.png
	rm -f out/aggregated-data/splom.png
	rm -f out/aggregated-data/marginal-distributions-2.rds
	rm -f out/ape-sim-figures-1.rds
	rm -f out/ape-sim-figures-2.rds
	rm -f out/ape-sim-figures-combined.png
	rm -f out/marginal-distributions-combined.png
	rm -f out/ape-sim-figures.rds
