<!-- README.md is generated from README.Rmd. Please edit that file -->



# The US Presidential Election

<!-- badges: start -->
[![lint-project.yaml](https://github.com/aleksanderbl29/US_pres_elec/actions/workflows/lint-project.yaml/badge.svg)](https://github.com/aleksanderbl29/US_pres_elec/actions/workflows/lint-project.yaml)
<!-- badges: end -->

This repo is meant to organise all my files for the course ***232E24 - The US Presidential Election - Voter Behaviour and Simulation Models***.

It was first used as a place to store all the misc. code for the course. The last place that was done is commit `92a65f0beaeb118778e818ecb7b993f9d5ee9126`.

It is now written as a targets pipeline to make writing the synopsis easier. You should be able to replicate the entire thing with a simple `tar_make()` (and some more; read below).

The original model code can be found in `model.R`. The pipeline in `_targets.R` is adapted directly from there.

## Instructions to reproduce

1. Ensure all packages are installed `renv::restore()`.
2. Run the `run.R` script.
3. Enjoy reading the synopsis, looking at the graphs or build on the model yourself.
