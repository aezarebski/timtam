
SEED=4
DURATION=40.0
PARAM_JSON=../example-parameters.json

index.html: src/make-viewer.R out/posterior-marginals.png
	Rscript src/make-viewer.R

out/simulation-data.json: ../example-parameters.json ape-sim.R
	./ape-sim.R --seed ${SEED} -p ${PARAM_JSON} -o out --duration ${DURATION} --make-plots -v
	Rscript src/prep-mcmc-data.R

out/mcmc-samples.csv: out/simulation-data.json
	stack exec -- mcmc out/simulation-data.json

out/posterior-marginals.png: out/mcmc-samples.csv src/mcmc-plots.R
	Rscript src/mcmc-plots.R

clean:
	rm -f out/ape-sim-event-times.csv
	rm -f out/ape-simulation-figure.png
	rm -f out/mcmc-diagnostics.json
	rm -f out/mcmc-samples.csv
	rm -f out/mcmc-traceplot-*.png
	rm -f out/posterior-marginals.png
	rm -f out/posterior-r-naught.png
	rm -f out/simulation-data.json
	rm -f index.html
