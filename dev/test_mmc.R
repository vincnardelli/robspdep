library(spdep)
set.seed(1234)
e<-rnorm(100)
nb<-cell2nb(10,10, "rook")
w<-nb2listw(nb)
ww<-nb2mat(nb, style="B")
I<-diag(1,100,100)
r<-0.7
x<-(solve(I-r*ww))%*%e


S0 <- spdep::Szero(w)
S0
sum(diag((t(ww) + ww)%*%ww))^(-1/2)
