#' Permutation test for Robust Geary's C statistic
#'
#'
#'  A permutation test for Robust Geary's C statistic calculated by using nsim random
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
#' @importFrom boot boot
#' @importFrom spdep Szero
#'
#' @examples
#' robustgeary(sampledata$x,sampledata$w)

robustgeary <- function(x, listw, alternative="greater", nsim=999) {

  robustgeary_fn <- function(x, listw, n, S0) {
    n1 <- n-1
    x <- c(x)
    xx <- mean(x)
    z <- x - xx
    zz <- sum(z^2)
    K <- (length(x)*sum(z^4))/(zz^2)

    mat <- spdep::listw2mat(listw)
    zs <- expand.grid(z, z)
    zs_sum <- abs(zs[,1] - zs[,2])
    C <- (n1 / S0) * sum(abs(zs[,1] - zs[,2])*as.numeric(mat)) / sum(abs(z - mean(z)))

    res <- list(C=C, K=K)
    res
  }

  robustgeary_boot <- function(var, i, ...) {
    var <- var[i]
    return(robustgeary_fn(x=var, ...)$C)
  }

  n <- length(listw$neighbours)
  S0 <- spdep::Szero(listw)

  res <- boot::boot(x, statistic=robustgeary_boot, R=nsim,
                sim="permutation", listw=listw, n=n, S0=S0)

  res <- numeric(length=nsim+1)
  for (i in 1:nsim) res[i] <- robustgeary_fn(sample(x), listw, n, S0)$C
  res[nsim+1] <- robustgeary_fn(x, listw, n, S0)$C
  rankres <- rank(res)
  xrank <- rankres[length(res)]
  diff <- nsim - xrank
  diff <- ifelse(diff > 0, diff, 0)
  if (alternative == "greater"){
    pval <- punif((diff + 1)/(nsim + 1), lower.tail = FALSE)
  }else if (alternative == "less"){
    pval <- punif((diff + 1)/(nsim + 1))
  }

  statistic <- res[nsim + 1]
  statistic
  pval

  ret <- list(statistic = statistic,
              p.value = pval,
              res = res)
  class(ret) <- "robspdep.robustgeary"
  return(ret)

}


