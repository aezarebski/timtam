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

The output of `pip freeze` for the virtual environment being used is here.

```
cycler==0.10.0
Cython==0.29.21
ete3==3.1.1
kiwisolver==1.1.0
matplotlib==3.0.3
numpy==1.18.5
pkg-resources==0.0.0
pyparsing==2.4.7
python-dateutil==2.8.1
scipy==1.4.1
six==1.15.0
```

## Parameters

The parameters used in this computation are (unfortunately) repeated in a few
places, so if you want to change them you need to check they are consistent
across several files.

- `src/prepare-simulations-for-popsize.R`
- `../../apps/timing-evaluation/Main.hs`
- `popsize-distribution/timing.py`

## Running

REMEMBER TO SOURCE THE VIRTUAL ENVIRONMENT FIRST!

```
stack clean 
stack build 
rm out/*
rm fibber.html 
rm fobber.csv 
stack exec -- timing-evaluation --output fibber.html --csv fobber.csv --time-limit 5 
Rscript src/prepare-simulations-for-popsize.R 
cd popsize-distribution 
./run-python-timing.sh
cd ../ 
Rscript src/plot-profiles.R
Rscript src/plot-llhds.R
```

## Results

Since the whole point of this is the timing of the two methods, lets look first
at how the timings compare. Note that this figure is produced by
`src/plot-profiles.R`.

![](out/profiles.png)

But of course, the benefits of a faster algorithm are only meaningful if it
gives the correct results so lets look at a comparison of the LLHD across the
two methods. There appears to be an additive constant that differs between the
two methods, but this wsa also present in Marc's code so I suspect there is
something about nnumerical stability in his code that accounts for this.

![](out/llhd-comparison.png)

Finally, let's consider how the selected truncation parameter differs with the
size of the data set, since this is a novel result too.

![](out/out/truncation-comparison.png)

