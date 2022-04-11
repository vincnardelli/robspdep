library(spdep)
library(robspdep)

moran.mc(sampledata$x, sampledata$w, nsim=999)
moran.mc(sampledata$y, sampledata$w, nsim=999)

robustmoran(sampledata$x, sampledata$w)
robustmoran(sampledata$y, sampledata$w)

moranhuber(sampledata$x,sampledata$w)
moranhuber(sampledata$y,sampledata$w)

localmoran(sampledata$x, sampledata$w)
localrobustmoran(sampledata$x, sampledata$w)
localmoranhuber(sampledata$x, sampledata$w)
