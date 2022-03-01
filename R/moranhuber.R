#' Permutation test for Moran-Huber's I statistic
#'
#'
#'  A permutation test for Moran's I statistic calculated by using nsim random
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
#'
#' @examples
#' moranhuber(sampledata$x,sampledata$w)


moranhuber <- function(x, listw, alternative = "greater", nsim=999){

  moranhuber_boot <- function(x, i, x_lag, listw, nsim=nsim, ...) {
    x <- x[i]
    a<-1/mad(x)
    x_lag<- med.lag.listw(listw=listw,x=x)
    b<-1/mad(x_lag)
    madsum <- mad(a*x+b*x_lag)^2
    maddif <- mad(a*x-b*x_lag)^2
    num<-madsum-maddif
    den<-madsum+maddif
    return(num/den)
  }


  x_lag<-med.lag.listw(listw=listw,x=x)

  res3 <- boot::boot(x, statistic = moranhuber_boot,
               R = nsim,
               sim = "permutation",
               listw = listw,
               x_lag=x_lag)


  a<-1/mad(x)
  b<-1/mad(x_lag)
  num<-(mad(a*x+b*x_lag)^2)-(mad(a*x-b*x_lag)^2)
  den<-(mad(a*x+b*x_lag)^2)+(mad(a*x-b*x_lag)^2)
  true <- num/den
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
  class(ret) <- "robspdep.moranhuber"

  return(ret)
}
