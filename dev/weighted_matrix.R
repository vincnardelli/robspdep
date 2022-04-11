# matrice con mediana pesata

x <- rnorm(100)
coords <- cbind(runif(100), runif(100))
listw <- nb2listw(knn2nb(knearneigh(coords, 3)),
                  style= "C")
#listw <- nb2listw(dnearneigh(coords, 0, 0.2), style="S")
#listw

listw$neighbours[1]
listw$weights[1]
listw$weights[2]
sapply(listw$weights, function(x) mean(x[1]==x))
lag.listw()
sapply(listw$neighbours, length)
med.lag.listw(listw, x)
m1 <- round(sapply(listw$neighbours, function(idx) median(x[idx])), 2)
m2 <- round(sapply(1:length(listw$neighbours), function(idx) matrixStats::weightedMedian(x=x[listw$neighbours[[idx]]], w=listw$weights[[idx]])), 2)

which(m1 != m2)

median(x)
matrixStats::weightedMedian(x, runif(100, 0, 0.1))

m1
m2
