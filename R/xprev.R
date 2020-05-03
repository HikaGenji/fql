xprev <- function(l, k) {
  if(k==0) {
    return(l)
  }
  if(k>=0) {
    return(c(rep(NA,min(k, length(l))), head(l, -min(k, length(l)))))
  } else {
    return(c(tail(l, max(k, -length(l))), rep(NA, min(-k, length(l)))))
  }
}