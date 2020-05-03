\name{select}
\alias{select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functional select
}
\description{
select whatExpression by byExpression from d where whereExpression
order of evaluation from, where, by, what
}
\usage{
select(d, where = NULL, by = NULL, what = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{where}{
A list of constraints expressions evaluating to booleans
}
  \item{by}{
A list of groupby expression
}
  \item{what}{
A list of aggregate expressions
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe
}
\references{

}
\author{
JFP
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d <- data.frame(
    date=seq(as.Date("2001-01-01"), as.Date("2001-12-01"), by="month"),
    ticker=sample(c("1 HK", "2 HK"), 12, TRUE),
    return=rnorm(12)
  )
d \%>\% select(by=list(ticker=expression(ticker))) # defaut by to take last
d \%>\% select(by=list(ticker=expression(ticker)),
               what=list(m=expression(mean(return)),s=expression(sd(return))))
d \%>\% update(by=list(ticker=expression(ticker)),
               what=list(m=expression(mean(return)),s=expression(sd(return))))
d \%>\% select(where=list(expression(return>0)),
               by=list(ticker=expression(ticker)),
               what=list(m=expression(mean(return)),s=expression(sd(return))))
d \%>\% update(where=list(expression(return>0)),
               by=list(ticker=expression(ticker)),
               what=list(m=expression(mean(return)),s=expression(sd(return))))
d \%>\% update(where=list(expression(return>0)), 
               by=list(ticker=expression(ticker)), 
               what=list(m=expression(mean(return)),s=expression(sd(return))))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{select}
\keyword{query}% __ONLY ONE__ keyword per line
