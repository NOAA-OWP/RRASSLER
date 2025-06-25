> All notable changes to this project will be documented in this file.  We attempt to follow the [Semantic Versioning 2.0.0](http://semver.org/) format.

# RRASSLER 0.1.4  - 2024-06-04

## Changes

Merged main branch, code spellcheck, package organelles, and documentation refresh; verified `devtools::check()` passes.  One new function addition, [insert_manual_record](https://noaa-owp.github.io/RRASSLER/reference/insert_manual_record.html), which lets you manually place an otherwise unparseable model into a RRASSLER record format.   See the [function references](https://noaa-owp.github.io/RRASSLER/reference/insert_manual_record.html) for more details.

# RRASSLER 0.1.3  - 2024-04-06

## Changes

Update to better conform to package documentation standards and general cleanup.

# RRASSLER 0.1.2  - 2024-12-18

## Changes

More documentation polish and the reintroduction of package heavy functionality and formatting.  Check out a new, in-line unit test pattern [here](https://noaa-owp.github.io/RRASSLER/articles/enhancement_accountinglock.html).

# RRASSLER 0.1.1  - 2024-12-06

## Changes

Mini-fied and slimmed down the entire package into a hydrofabric-verse compatible format so that it can pass `devtools::check()` for a premier at AGU.  There will be another PR following that re-adds those temporarily lost functions in a more R package appropriate form shortly.

# RRASSLER 0.1.0  - 2024-06-28

## Changes

We have reached a stable minor version that addresses parsing issues, model accounting quirks, and stability.  With this set of enhancements, the core logic that RRASSLER deploys is not expected to need major deviation for the foreseeable future.  

- All model files are now copied into each geometric realization of the model to accommodate multi-plan model types
- Doc cleanup, code tightening and removal of extra items.  Updated visuals and pkgdown functions.

# RRASSLER 0.0.02 - 2023-11-08

## Changes

- Removed multiple dependencies, hull cleanup, addressed todos and typos, updated docs.

# RRASSLER 0.0.01 - 2023-07-25

## Added

- First large release.
