# ChangeLog

## 0.1.1.0

- Switch to types imported from `epi-types`.

## 0.1.0.1

- Return `-Infinity` for impossible parameters rather than throwing an error.
  This is acheived by wrapping the likelihood function to recognise impossible
  parameters before doing any real computation.
- Update to the latest version of `epi-sim` which had a couple of breaking
  changes.

## 0.1.0.0

- Initial commit in new repository the likelihood and some example scripts.
