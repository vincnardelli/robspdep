#' @export
print.robspdep.moranhuber <- function(x, ...){
  cat("Monte-Carlo simulation of Moran-Huber I \n")
  cat("Number of simulations: ", length(x$res), "\n")
  cat("statistic = ", x$statistic, "p-value =", x$p.value)
}
#' @export
print.robspdep.robustmoran <- function(x, ...){
  cat("Monte-Carlo simulation of Robust Moran I \n")
  cat("Number of simulations: ", length(x$res), "\n")
  cat("statistic = ", x$statistic, "p-value =", x$p.value)
}
