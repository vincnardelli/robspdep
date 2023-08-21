library(spdep)
library(robspdep)

x <- sampledata$x
listw <- sampledata$w

n <- length(listw$neighbours)
xx <- mean(x)
z <- x - xx
x_lag <- med.lag.listw(listw, z)
lz <- lag.listw(listw, x)

mat <- listw2mat(listw)
trW2 <- sum(diag(mat %*% mat))

# APLE
num <- t(lz)%*%z + t(z)%*%lz
den <- t(lz)%*%lz+trW2*t(z)%*%z/n

num/den/2

# RAPLE
numr <- t(x_lag)%*%z + t(z)%*%x_lag
denr <- t(x_lag)%*%x_lag+trW2*t(z)%*%z/n

numr/denr/2

t(z)%*%z
sum(z^2)


