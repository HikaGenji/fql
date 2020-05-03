uj <- function(x, y) {
  if(is.null(attributes(y)$key) & is.null(attributes(x)$key)) {
    return(merge(x,y, all=TRUE))
  } else {
    return(lj(x, y, keep.y=TRUE))
  }
}