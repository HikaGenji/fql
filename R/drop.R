#' remove first y values from x
#' @param x a vector
#' @param y an integer
#' @retun vector with y values dropped from front of x
`%_%` <- function(x, y) {
  tail(y, -x)
}
