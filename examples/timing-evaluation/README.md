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
