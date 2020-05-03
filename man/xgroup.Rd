\name{xgroup}
\alias{xgroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group a dataframe into a list of dataframe along keys
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xgroup(d, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{cols}{
A character vector of column keys
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of dataframe 
}
\references{

}
\author{
JFP
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  d <- rbind(data.frame(date="20140101", ticker= 2, price=3),
             data.frame(date="20140102", ticker= 2, price=2),
             data.frame(date="20140101", ticker= 1, price=5))
  d \%>\% xcols(c("ticker", "price"))
  # xdesc, xcol, xcols
  d \%>\% xdesc(c("date", "price")) %>% xcol("dat") %>% xcols("price")
  # xgroup and each
  d \%>\% xgroup(c("date", "price")) %>% each(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{xgroup}

