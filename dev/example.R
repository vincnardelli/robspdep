library(spdep)
library(robspdep)

moran.mc(sampledata$x, sampledata$w, nsim=999)
moran.mc(sampledata$y, sampledata$w, nsim=999)

robustmoran(sampledata$x, sampledata$w)
robustmoran(sampledata$y, sampledata$w)

gk(sampledata$x,sampledata$w)
gk(sampledata$y,sampledata$w)
