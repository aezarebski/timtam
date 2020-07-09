

out/computed-values.csv:
	stack exec -- manceau-comparison

out/manceau-comparison.pdf: out/computed-values.csv src/plot-comparison.R
	Rscript src/plot-comparison.R

figure: out/manceau-comparison.pdf

clean:
	rm -f out/manceau-comparison.pdf
	rm -f out/computed-values.csv
