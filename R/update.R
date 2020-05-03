recycle <- function(x, cols, base) {
  l     <- lapply(cols, function(y) eval(y, env = x, enclos = base))
  data.frame(l)
}

update <-  function (d, where = NULL, by = NULL, what = NULL) 
{
  if (is.null(d)) 
    return(d)
  if (nrow(d) == 0) 
    return(d)
  base <- parent.frame(4)
  if ((length(where) == 0) | is.null(where)) {
    where <- list(rep(TRUE, nrow(d)))
  }
  if (length(what) == 0 | is.null(what)) {
    if (!is.null(by)) {
      what <- lapply(colnames(d), function(x) parse(text = paste("tail(", 
                                                                 x, ",1)", sep = "")))
      names(what) <- colnames(d)
    }
    else {
      what <- lapply(colnames(d), function(x) parse(text = x))
      names(what) <- colnames(d)
    }
  }
  if (!is.null(by)) {
    bye <- by
    nonby <- setdiff(colnames(d), names(bye))
    bye[nonby] <- lapply(nonby, function(x) parse(text = x))
  }
  else {
    bye <- lapply(colnames(d), function(x) parse(text = x))
    names(bye) <- colnames(d)
  }
  if(!is.null(by)) {
    by[names(by)]   <- lapply(names(by), function(x) parse(text=paste("tail(", x, ", 1)", sep="")))
    what            <- c(what, by)
  }
  cols <- lapply(colnames(d), function(x) parse(text = x))
  names(cols) <- colnames(d)
  cols[names(what)] <- what
  i <- Reduce("&", lapply(where, function(x) eval(x, env = d, 
                                                  enclos = base)))
  j <- dict(1:nrow(d), i)
  e <- d
  for (j in names(cols)) {
    if (!j %in% colnames(d)) {
      e[, j] <- NA
    }
  }
  if (nrow(d[i, , FALSE]) == 0) {
    return(d)
  }
  
  x <- d[i, , FALSE] %>% (function(x) data.frame(lapply(bye, 
                                                        function(y) eval(y, env = x, enclos = base)), stringsAsFactors = FALSE)) %>% 
    xgroup(names(by)) %>% each(function(x) recycle(x, cols, base)) %>% 
    ungroup
  e[i, ] <- x
  for(i in 1:ncol(e)) {
    class(e[, i]) <- class(x[, i])
  }
  if(!is.null(by)) {
    return(e %>% xcols(names(by)) %>% xasc(names(by)))
  } else {
    return(e)
  }
}