#' coalesce fills missing values
#' @param table a dataframe
#' @param value a value
#' @return dataframe whose missing values have been replaced by value
coalesce <- function(table, value) {
  table[is.na(table)] <- value
  table
}

`%^%` <- function(x, y) {
  coalesce(y, x)
}
