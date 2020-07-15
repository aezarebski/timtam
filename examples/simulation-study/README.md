# Simulation Study

This example simulates from the BDSCOD model and then generates figures showing
profiles of the log-likelihood about the true parameters used in the simulation.

```
make figures
```

All of the output files will appear in `out/`. The parameters used in the
simulation are specified in a JSON configuration file, `out/config.json` which
we generate with the script `src/json-config.R`. Note that the visualisation of
the simulation requires the `src/birth-death-lines.R` script which is not part
of this repository but can be copied over from the `epi-sim` package.
