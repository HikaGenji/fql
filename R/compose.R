#' function composition
#' @param ... functions
#' @return function
#'
compose <- function(...) {
  do.call(multi.argument.Compose, rev(list(...)))
}

'%o%' <- function(f, g) {
  compose(f, g)
}
