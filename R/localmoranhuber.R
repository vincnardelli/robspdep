#' Local Moran-Huber's I statistic
#'
#' The local spatial statistic Moran-Huber's I is calculated for each zone based on the spatial weights object used.
#' The values returned include a Z-value, and may be used as a explorative analysis tool.
#'
#' @param x a numeric vector the same length as the neighbours list in spdep listw
#' @param listw a \code{listw} object created with spdep for example by \code{spdep::nb2listw}
#' @param nsim number of permutations
#'
#' @return A list with letters and numbers.
#' \itemize{
#'   \item Ii - local moran-huber statistic
#'   \item E.Ii	- permutation sample mean
#'   \item Var.Ii - permutation sample standard deviation
#'   \item Z.Ii - permutation sample means and standard deviations
#'   \item Pr - P-value
#' }
#' @export
#' @importFrom stats pnorm var
#' @importFrom spdep card
#' @examples
#' localmoranhuber(sampledata$x, sampledata$w)

localmoranhuber <- function(x, listw, nsim=1000){

  crd <- card(listw$neighbours)
  x_lag<-med.lag.listw(listw=listw,x=x)
  z <- x-median(x)
  z_lag <- med.lag.listw(listw, z)
  I <- (z * z_lag)/(mad(z)*mad(z_lag))


  locmo <- function(i){
    zi <- z[i]
    z_i <- z[-i]
    crdi <- crd[i]

    sz_i <- matrix(sample(z_i, size = crdi * nsim, replace = TRUE),
                   ncol = crdi, nrow = nsim)

    z_lag_s <- apply(sz_i, 1, median)
    Ii <- I[i]
    Is <- (zi * z_lag_s)/(mad(z)*mad(z_lag))

    m <- mean(Is)
    v <- var(Is)
    z_score <- (Ii - m)/sqrt(v)
    p <- 2 * pnorm(abs(z_score), lower.tail = FALSE)

    Is[nsim + 1] <- Ii
    rankres <- rank(Is)
    xrank <- rankres[length(Is)]
    diff <- nsim - xrank
    diff <- ifelse(diff > 0, diff, 0)
    pval <- punif(abs(xrank - (nsim + 1)/2)/(nsim + 1),
                  0, 0.5, lower.tail = FALSE)

    return(c(Ii, m, v, z_score, pval))
  }

  res <- lapply(1:length(x), locmo)
  res <- do.call(rbind, res)
  colnames(res) <- c("Ii","E.Ii", "Var.Ii", "Z.Ii", "Pr(z != E(Ii))")
  class(res) <- "robspdep.localmoranhuber"

  return(res)
}




