# ChangeLog

## 0.1.4.0

- When looking for scheduled events at a particular time, search for times less
  than 1e-13 different rather than using equality to prevent numerical errors
  causing them to be missed. See `timesWithinEpsilon` function in
  `BDSCOD.Utility`.
- Improve documentation and read shared parameters from a single file.
- Replace `manceau-comparison` with an example looking at using data which
  starts from the TMRCA rather than the origin.
- Move the `MCMCConfiguration` type into `BDSCOD.Types` so it can be used in
  multiple applications.
- Add some orphan instances of useful typeclasses which are missing from the
  current version of `epi-sim`.
- Update to `epi-sim-0.3.0.0` which is on github and remove the `Condition`
  module since it is pretty useless.

## 0.1.3.1

- The application `simulation-study-aggregated-observations` now conditions the
  simulations to avoid the LTT going back down to 1 after it has been larger
  than 1 to avoid potential issues with multiple origin times, and that the MCMC
  has been able to actually generate some output.
- There is now another example `prevalence-ci-calibration` which looks at the
  coverage properties of the CI under the approximate likelihood.

## 0.1.3.0

- The calculation of the likelihood surface cross sections has been replaced by
  a proper MCMC analysis in the aggregated data simulation study. This
  introduces a dependency upon `might-metropolis`.
- The aggregation simulation study now accepts a random seed from the
  configuration file and assumes there will be a positive rate for unscheduled
  unsequenced sampling.
- **Important** The aggregation simulation study now ignores any occurrence
  events that occur after the last unscheduled sequenced sample.
- **Important** Change the behaviour of the aggregation functions to remove any
  unscheduled events that occur after the last aggregation time.
- Rewrite the aggregation functionality and use QuickCheck to make sure that
  this works as expected.
- Change the constructor for `AggregationTimes` to include the type of event
  that is being aggregated at that point in time.

## 0.1.2.16

- Do the likelihood calculation in log-space now where possible and introduce
  `logSumExp` to assist with this. This has been checked with QuickCheck which
  is now a dependency.
- Use a fixed seed in the aggregated observations simulation study to make this
  easier to debug and include a regularisation term in the MLE computation to
  prevent some numerical errors from parameters wandering off.
- Move `logit` and `invLogit` into the `Utility` module out of the applications
- Include the `Aggregation` module for working with aggregated unscheduled
  observations.

## 0.1.2.15

- Fix the seed used when simulating the data to ensure that the results are
  reproducible and reduce to only two inference times.
- Include the size of the simulation that is generated at each observation time
  in the time series simulation.

## 0.1.2.14

- Include the estimated parameter values in the log-likelihood cross-section
  figures.

## 0.1.2.13 

- Fetch `epi-sim` as a tarball from hackage rather than using the version on
  github. Remove dependency on `epi-types` and update the resolver used by
  stack.

## 0.1.2.12 

- Make the specification of the LLHD profiles in
  `examples/simulation-study-time-series` part of the configuration of the
  application and refactor some of the code for clarity while doing this.

## 0.1.2.11 

- Refactor a lot of the plotting code so that it is easier to understand and
  include the true simulation parameters in the output for
  `simulation-study-time-series`.
- Improved documentation for `examples/simulation-study-time-series`, swapping
  to an org-mode README in the process which generates a runner file, `run.sh`.

## 0.1.2.10 

- The `timing-evaluation` application now takes a JSON file for configuration
  rather than relying on hard coded values. The same configuration file is used
  by the population size python code too. The documentation in the example has
  been improved to make it easier to tweak the simulation settings in the
  future.

## 0.1.2.9

- Tweak visualisation of the prevalence estimates for clarity.
- The commits for both `epi-sim` and `epi-types` have been updated.
- There is now a `Parameters` type rather than just an alias and there are some
  setter functions to help manipulate these.
- The way that the likelihood profiles are generated in the
  `simulation-study-time-series` application is different now and are repeated
  for both the true parameters and estimated ones.
- Estimate the parameters given a simulated data set using `hmatrix` and
  `hmatrix-gsl`. The simplex method appears to do a good job and does not
  require gradients, the simulated annealing method was too slow. The death rate
  is assumed known a priori.

## 0.1.2.8

- Do more of the likelihood calculation in log-space to avoid numerical errors
  and test the new functions against the old ones.
- Include a simulation study which tracks the prevalence through time and allows
  for more flexible study of the use of scheduled data. See
  `./examples/simulation-study-time-series/` for the details of this.
- Update to `epi-types-0.2.1.2` and `epi-sim-0.2.0.1` to get a bug fix.

## 0.1.2.7

- Generate a LaTeX table of the simulation parameters.
- Refactor the =simulation-study= example for clarity.
- Fix a bug in the likelihood which appeared when there are observations after a
  scheduled event.

## 0.1.2.6

- Include an example looking at the computational complexity of the BDSCOD and
  the algorithm from Marc Manceau et al (2020).

## 0.1.2.5

- Include a `manceau-comparison` example and include a figure of the data that
  is used in the comparison.

## 0.1.2.4

- Include Newick output from `simulation-study` and generate a very simple tree.
- Update to newer versions of `epi-sim` and `epi-types`.

## 0.1.2.3

- Adjust the figures generated in `simulation-study`.

## 0.1.2.2

- Include a visualisation of the simulated data set used in `simulation-study`
- Use a JSON file to configure the `simulation-study` and allow for multiple
  \(\rho\) events to occur. This adds an additional dependency on `aeson`.

## 0.1.2.1

- Use `make` to run the `simulation-study`.

## 0.1.2.0

- Include the `BDSCOD.Conditioning` module for helper functions for conditioning
  the likelihood.
- Document more of the functionality in `BDSCOD.Llhd`.

## 0.1.1.0

- Include additional testing.
- Upgrade the version of `epi-sim`
- There is an application for exploring the behaviour of the `InhomogeneousBDS`:
  details are in the `examples` directory.
- Implement the likelihood for the birth-death-sampling model with inhomogeneous
  birth rate making use of the functions in `BDSCOD.Llhd` to do the actual
  computation.
- Switch to types imported from `epi-types` and create a `BDSCOD.Types` module
  for types shared between constant and variable rate likelihoods.

## 0.1.0.1

- Return `-Infinity` for impossible parameters rather than throwing an error.
  This is acheived by wrapping the likelihood function to recognise impossible
  parameters before doing any real computation.
- Update to the latest version of `epi-sim` which had a couple of breaking
  changes.

## 0.1.0.0

- Initial commit in new repository the likelihood and some example scripts.
