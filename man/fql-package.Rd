\name{fql-package}
\alias{fql-package}
\alias{fql}
\docType{package}
\title{
Functional queries for R
}
\description{
Modelled after kdb/q, a replacement for dplyr, data.table and the likes. Pure R implementation, 0 dependency on external package.
}
\details{
\tabular{ll}{
Package: \tab fql\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-10-24\cr
License: \tab 4 (pun intended)\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
JFP

Maintainer: jperreton@gmail.com
~~ The author and/or maintainer of the package ~~
}
\references{
}

\keyword{query}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
  d <- data.frame(
    date=seq(as.Date("2001-01-01"), as.Date("2001-12-01"), by="month"),
    ticker=sample(c("1 HK", "2 HK"), 12, TRUE),
    price=100+rnorm(12)
  )
  
  e <- data.frame(ticker=c("1 HK", "2 HK"), sector=c("CD", "IT")) %>% xkey("ticker")
  
  d %>% 
    xasc(c("date", "ticker")) %>%
    update(
      by=list(ticker=expression(ticker)),
      what=list(return=expression(price/xprev(price,1)-1))
    ) %>%
    lj(e) %>%
    update(by=list(ticker=expression(ticker)),
           what=list(avgPx=expression(rply(price,3,mean)))) 
}
