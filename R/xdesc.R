xdesc <- function (d, cols) {
  s <- paste("with(d, order(-", paste("rank", "(", cols, ")", sep="", collapse=","),
             "))", sep = "")
  d[eval(parse(text = s)), ]
}