xcol <- function(d, cols) {
  names <- colnames(d)
  names[1:length(cols)] <- cols
  colnames(d) <- names
  d
}