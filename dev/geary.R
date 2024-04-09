library(robspdep)
library(boot)
x <- sampledata$x
listw <- sampledata$w

robustmoran(x, listw)
robustgeary(x, listw)
robustaple(x, listw)

