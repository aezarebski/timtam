
figures: out/profiles.pdf

out/simulation-sizes-and-llhds.csv:
	stack exec -- timing-evaluation --output fibber.html --csv fobber.csv --time-limit 10

out/profiles.pdf: out/simulation-sizes-and-llhds.csv src/plot-profiles.R
	Rscript src/plot-profiles.R




