\name{xcols}
\alias{xcols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reorder columns in a table. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xcols(d, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe, with reordered columns
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
