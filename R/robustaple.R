#' Permutation test for Robust APLE statistic
#'
#'
#'  A permutation test for Robust APLE statistic calculated by using nsim random
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
#' @importFrom stats mad median punif
#' @importFrom spdep listw2mat
#'
#' @examples
#' robustaple(sampledata$x,sampledata$w)


robustaple <- function(x, listw, alternative = "greater", nsim=999){

  robustaple_boot <- function(x, i, x_lag, listw, nsim=nsim, ...) {
    x <- x[i]
    xx <- mean(x)
    z <- x - xx
    x_lag <- med.lag.listw(listw, z)
    num <- t(x_lag)%*%z + t(z)%*%x_lag
    den <- t(x_lag)%*%x_lag+trW2*t(z)%*%z/n
    return(num/den/2)
  }


  n <- length(listw$neighbours)
  xx <- mean(x)
  z <- x - xx
  x_lag <- med.lag.listw(listw, z)
  mat <- spdep::listw2mat(listw)
  trW2 <- sum(diag(mat %*% mat))

  res3 <- boot::boot(x, statistic = robustaple_boot,
                     R = nsim,
                     sim = "permutation",
                     listw = listw,
                     n=n,
                     trW2=trW2)

  num <- t(x_lag)%*%z + t(z)%*%x_lag
  den <- t(x_lag)%*%x_lag+trW2*t(z)%*%z/n
  true <- num/den/2
  true

  res <- res3$t
  res[nsim + 1] <- true
  rankres <- rank(res)
  xrank <- rankres[length(res)]
  diff <- nsim - xrank
  diff <- ifelse(diff > 0, diff, 0)
  if (alternative == "less")
    pval <- punif((diff + 1)/(nsim + 1), lower.tail = FALSE)
  else if (alternative == "greater")
    pval <- punif((diff + 1)/(nsim + 1))

  statistic <- res[nsim + 1]
  statistic
  pval

  ret <- list(statistic = statistic,
              p.value = pval,
              res = res)
  class(ret) <- "robspdep.robustaple"

  return(ret)
}
