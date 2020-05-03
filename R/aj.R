#' as of join
#' @param by is a list of columns
#' @param x is a dataframe
#' @param y is dataframe
#' @return dataframe which is joining y to x as of by columns
aj <- function(by, x, y) {
  tofill         <- setdiff(colnames(y), by)
  filling        <- lapply(tofill, function(x) parse(text=paste("fills(", x, ")")))
  names(filling) <- tofill
  toby           <- lapply(head(by, -1), function(x) parse(text=x))
  names(toby)    <- head(by, -1)
  l <- y %>%
    xasc(by) %>%
    uj(x[, by, drop=FALSE] %>%
         xkey(by)) %>%
    xasc(by)
  if(length(toby)==0) {
    l <- l %>% update(what=filling)
  } else {
    l <- l %>% update(by=toby, what=filling) 
  }
  x %>% lj(l %>% xkey(by))
}