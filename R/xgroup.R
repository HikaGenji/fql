xgroup <- function(d, cols=NULL) {
  key <- paste(paste("(",paste(cols,collapse=","), ")", sep=""),
                 "!", paste("(", apply(d[, cols,FALSE], 1, paste, collapse=","), ")", sep=""))
  by(d, key, function(x) x)
}