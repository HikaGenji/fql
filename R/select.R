
select <- function (d, where = NULL, by = NULL, what = NULL) {
  base <- parent.frame()
  if ((length(where) == 0) | is.null(where)) {
    where <- list(rep(TRUE, nrow(d)))
  }
  if (length(what) == 0 | is.null(what)) {
    if (!is.null(by)) {
      what        <- lapply(colnames(d), function(x) parse(text = paste("tail(", x, ", 1)", sep = "")))
      names(what) <- colnames(d)
    }
    else {
      what        <- lapply(colnames(d), function(x) parse(text = x))
      names(what) <- colnames(d)
    }
  }
  if(length(names(what))==0) {
    if(all(what %>% each(class) %>% unlist =="name")) {
      names(what) <- unlist(what)
    }
  }
  if(length(names(by))==0) {
    if(all(by %>% each(class) %>% unlist =="name")) {
      names(by) <- unlist(by)
    }
  }
  if (!is.null(by)) {
    bye   <- by
    nonby <- setdiff(colnames(d), names(bye))
    if (length(nonby) > 0) {
      bye[nonby] <- lapply(nonby, function(x) parse(text = x))
    }
  }
  else {
    bye        <- lapply(colnames(d), function(x) parse(text = x))
    names(bye) <- colnames(d)
  }
  r <- d[Reduce("&", lapply(where, function(x) eval(x, env = d, enclos = base))), , FALSE]
  if (nrow(r) == 0) {
    return(r)
  }
  if(!is.null(by)) {
    by[names(by)]   <- lapply(names(by), function(x) parse(text=paste("tail(", x, ", 1)", sep="")))
    what            <- c(what, by)
    what            <- what[!(as.character(what) %>% duplicated())]
  }
  r <- r %>%
    (function(x) data.frame(lapply(bye, function(y) eval(y, env = x, enclos = base)), stringsAsFactors = FALSE)) %>% 
    xgroup(names(by)) %>%
    each(function(x) data.frame(lapply(what, function(y) eval(y, env = x, enclos = base)), stringsAsFactors = FALSE)) %>% 
    ungroup
  if(!is.null(by)) {
    return(r %>% xcols(names(by)) %>% xasc(names(by)))
  } else {
    return(r)
  }
}

