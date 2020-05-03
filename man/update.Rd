\name{update}
\alias{update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update a dataframe, functional style
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update(d, where = NULL, by = NULL, what = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{where}{
A list of expressions evaluating to booleans
}
  \item{by}{
A list of expressions defining the groups
}
  \item{what}{
A list of aggregates expressions
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe, same number of rows as original d object
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
\code{\link{select}}
}
\examples{
d <- data.frame(
    date=seq(as.Date("2001-01-01"), as.Date("2001-12-01"), by="month"),
    ticker=sample(c("1 HK", "2 HK"), 12, TRUE),
    return=rnorm(12)
  )
d \%>\% update(by=list(ticker=expression(ticker)), what=alist(return=2)) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{query}
\keyword{update}% __ONLY ONE__ keyword per line
