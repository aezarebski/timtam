# ChangeLog

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
