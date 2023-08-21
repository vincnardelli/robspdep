#' Permutation test for Robust aple's I statistic
#'
#'
#'  A permutation test for Robust aple's I statistic calculated by using nsim random
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
#' robustaple(sampledata$x,sampledata$w)
#'

library(spatialreg)
aple.mc()

robustaple <- function(x, listw, alternative="greater", nsim=999) {

  robustaple_fn <- function(x, listw, n) {
    z <- as.vector(scale(x, scale=F))
    lz <- med.lag.listw(listw, z)
    mat <- spdep::listw2mat(listw)
    n <- dim(mat)[1]
    trWW <- sum(diag(mat %*% mat))
    corterm <- (trWW/n) * diag(n)
    WU <- ((mat + t(mat))/2)
    W2 <- crossprod(mat) + corterm
    xwx <- crossprod(lz, (WU %*% z))
    xwwx <- crossprod(lz, (W2 %*% z))
    res <- c(as.matrix(xwx/xwwx))
    res
  }

  robustaple_boot <- function(var, i, ...) {
    var <- var[i]
    return(robustaple_fn(x=var, ...))
  }

  n <- length(listw$neighbours)

  res <- boot(x, statistic=robustaple_boot, R=nsim,
                sim="permutation", listw=listw, n=n)

  res <- numeric(length=nsim+1)
  for (i in 1:nsim) res[i] <- robustaple_fn(sample(x), listw, n)
  res[nsim+1] <- robustaple_fn(x, listw, n)
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
  class(ret) <- "robspdep.robustaple"
  return(ret)

}


