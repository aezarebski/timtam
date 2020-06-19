
figures: out/simulated-observations.json out/popsize-distribution-data.json

out/simulated-observations.json:
	stack build
	stack exec -- timing-evaluation

out/popsize-distribution-data.json: out/simulated-observations.json config.json src/prepare-simulation-for-popsize-distribution.R
	Rscript src/prepare-simulation-for-popsize-distribution.R config.json

clean-files:
	rm -f out/simulated-observations.json
	rm -f out/criterion-report.json
	rm -f out/popsize-distribution-data.json

clean-stack:
	stack clean

clean: clean-files clean-stack
