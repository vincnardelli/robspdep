# robspdep: Robust Spatial Dependence

<!-- badges: start -->
<!-- badges: end -->

The R package **robspdep** is a collection of functions and tests for robust spatial autocorrelation fully compatible with spdep. It include global and local Moran-Huber I.

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

moranhuber(sampledata$x,sampledata$w)
moranhuber(sampledata$y,sampledata$w)

localmoran(sampledata$x, sampledata$w)
localmoranhuber(sampledata$x, sampledata$w)

localmoran(sampledata$y, sampledata$w)
localmoranhuber(sampledata$y, sampledata$w)
```

