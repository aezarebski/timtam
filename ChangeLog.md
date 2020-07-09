# ChangeLog

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
