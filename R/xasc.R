# The sort is by the first column given, then by the second column within the first, and so on.
xasc <- function(d, cols) {
  s <- paste("with(d, order(",paste(cols,collapse=","),"))",sep="")
  d[eval(parse(text=s)), ]
  
}
