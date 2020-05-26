
figures: out/llhd-profile-lambda.png

out/llhd-profile-lambda.png: out/simulation-study-llhds.csv src/llhd-profiles.R
	Rscript src/llhd-profiles.R

out/simulation-study-llhds.csv:
	stack build
	stack exec -- simulation-study

clean:
	rm -f out/llhd-profile-*.pdf
	rm -f out/simulated-all-events.txt
	rm -f out/simulated-events-observed.txt
	rm -f out/simulated-events.txt
	rm -f out/simulation-study-llhds.csv
