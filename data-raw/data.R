library(spdep)
library(robspdep)
set.seed(1234)
e<-rnorm(100)
nb<-cell2nb(10,10, "rook")
w<-nb2listw(nb)
ww<-nb2mat(nb)
I<-diag(1,100,100)
r<-0.7
x<-(solve(I-r*ww))%*%e

# contamination
y<-x
y[18]<-50


sampledata <- list(x=as.numeric(x), y=as.numeric(y), w=w)
usethis::use_data(sampledata, overwrite = TRUE)
