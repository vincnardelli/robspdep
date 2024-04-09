# robspdep: Robust Spatial Dependence

<!-- badges: start -->
[![arXiv](https://img.shields.io/badge/arXiv-2402.07297-b31b1b.svg)](https://arxiv.org/abs/2402.07297)
<!-- badges: end -->

The R package **robspdep** is a collection of functions and tests for robust spatial autocorrelation fully compatible with spdep. It include global and local Robust Moran and GK presented in Nardelli and Arbia (2024).

## Installation
The package is work in progress and it is not available in the CRAN. However you can install it directly from the github repo.
``` r
# The easiest way to get robspdep is to install it from GitHub:
# install.packages("devtools")
devtools::install_github("vincnardelli/robspdep")
```

## Example of usage and comparison with spdep

This is a basic example which shows you how use robspdep and compare the results obtained with spdep:

``` r
library(spdep)
library(robspdep)

moran.mc(sampledata$x, sampledata$w, nsim=999)
moran.mc(sampledata$y, sampledata$w, nsim=999)

robustmoran(sampledata$x, sampledata$w)
robustmoran(sampledata$y, sampledata$w)

gk(sampledata$x,sampledata$w)
gk(sampledata$y,sampledata$w)
```


### References
Nardelli, Vincenzo, and Giuseppe Arbia. "On Robust Measures of Spatial Correlation." arXiv preprint arXiv:2402.07297 (2024).

