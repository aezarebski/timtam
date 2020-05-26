# Simulation Study

This example simulates from the BDSCOD model and then generates figures showing
profiles of the log-likelihood about the true parameters used in the simulation.

```
make figures
```

All of the output files will appear in `out/`. The parameters used in the
simulation are specified in a JSON configuration file, `out/config.json` which
we generate with the script `src/json-config.R`.
