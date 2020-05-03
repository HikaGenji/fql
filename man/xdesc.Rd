\name{xdesc}
\alias{xdesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort a dataframe in descending order by a (or more) column key(s)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xdesc(d, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{cols}{
A vector of character column names
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe, sorted in descending order by cols
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
  d <- data.frame(
    date=seq(as.Date("2001-01-01"), as.Date("2001-12-01"), by="month"),
    ticker=sample(c("1 HK", "2 HK"), 12, TRUE),
    price=100+rnorm(12)
  ) \%>\% xdesc("ticker")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
