# Timing Evaluation

This examples simulates trees of varying sizes using `epi-sim` and then times
how long it takes to evaluate the likelihood function on them. The results are
then plotted along with the corresponding times using an alternative likelihood
function.

```
make figures
```

Note that to generate the timing for the `popsize-distribuion` function, you
need to clone this repository into the current directory and then set up the
necessary python environment. Then after running the BDSCOD timing, you need to
adjust the simulation files and run `run-python-timing.sh` from withing
`popsize-distribution` to generate the timing results.
