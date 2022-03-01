#' Robust spatial lag of a numeric vector
#'
#' Using a \code{listw} sparse representation of a spatial weights matrix, compute the lag vector \eqn{W x}
#'
#' @param listw listw object
#' @param x a numeric vector the same length as the neighbours list in listw
#'
#' @return a numeric vector the same length as var
#' @export
#' @examples
#' med.lag.listw(sampledata$w,sampledata$x)

med.lag.listw <- function(listw, x){
  sapply(listw$neighbours, function(idx) median(x[idx]))
}
