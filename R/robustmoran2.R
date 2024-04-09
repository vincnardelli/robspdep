#' Permutation test for Robust Moran's I statistic 2
#'
#'
#'  A permutation test for Robust Moran's I statistic calculated by using nsim random
#'  permutations of x for the given spatial weighting scheme generated with spdep,
#'  to establish the rank of the observed statistic in relation to the nsim simulated values.
#'
#' @param x a numeric vector the same length as the neighbours list in spdep listw
#' @param listw a \code{listw} object created with spdep for example by \code{spdep::nb2listw}
#' @param alternative alternative a character string specifying the alternative hypothesis, must be "greater" (default), or "less".
#' @param nsim  number of permutations
#'
#' @return  A list with \code{statistic}, \code{p.value} and \code{res}
#' @export
#' @import boot
#' @importFrom spdep Szero
#'
#' @examples
#' robustmoran2(sampledata$x,sampledata$w)

robustmoran2 <- function(x, listw, alternative="greater", nsim=999) {

  robustmoran_fn <- function(x, listw, n, S0) {
    n1 <- length(listw$neighbours)
    x <- c(x)
    xx <- mean(x)
    z <- x - xx
    zz <- sum(abs(z))
    K <- (length(x)*sum(z^4))/(zz^2)
    lz <- med.lag.listw(listw, z)
    I <- (n / S0) * ((sum(z*lz)) / zz)
    res <- list(I=I, K=K)
    res
  }

  robustmoran_boot <- function(var, i, ...) {
    var <- var[i]
    return(robustmoran_fn(x=var, ...)$I)
  }

  cards <- card(listw$neighbours)
  n <- length(listw$neighbours)
  S0 <- spdep::Szero(listw)

  res <- boot(x, statistic=robustmoran_boot, R=nsim,
                sim="permutation", listw=listw, n=n, S0=S0)

  res <- numeric(length=nsim+1)
  for (i in 1:nsim) res[i] <- robustmoran_fn(sample(x), listw, n, S0)$I
  res[nsim+1] <- robustmoran_fn(x, listw, n, S0)$I
  rankres <- rank(res)
  xrank <- rankres[length(res)]
  diff <- nsim - xrank
  diff <- ifelse(diff > 0, diff, 0)
  if (alternative == "less"){
    pval <- punif((diff + 1)/(nsim + 1), lower.tail = FALSE)
  }else if (alternative == "greater"){
    pval <- punif((diff + 1)/(nsim + 1))
  }

  statistic <- res[nsim + 1]
  statistic
  pval

  ret <- list(statistic = statistic,
              p.value = pval,
              res = res)
  class(ret) <- "robspdep.robustmoran2"
  return(ret)

}


