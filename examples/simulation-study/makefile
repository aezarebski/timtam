
figures: out/parameter-llhd-profiles.pdf out/simulation-visualisation.png

out/simulation-visualisation.png: src/birth-death-lines.R out/simulated-events.csv
	Rscript src/birth-death-lines.R out/simulated-events.csv out/simulation-visualisation.png

out/parameter-llhd-profiles.pdf: out/simulation-study-llhds.csv src/llhd-profiles.R out/config.json
	Rscript src/llhd-profiles.R

out/simulation-study-llhds.csv: out/config.json
	stack build
	stack exec -- simulation-study

out/config.json: src/json-config.R
	Rscript src/json-config.R

clean:
	rm -f out/config.json
	stack clean
	rm -f out/parameter-llhd-profiles.pdf
	rm -f out/llhd-profile-*.pdf
	rm -f out/simulated-all-events.txt
	rm -f out/simulated-events-observed.txt
	rm -f out/simulated-events.txt
	rm -f out/simulation-study-llhds.csv
