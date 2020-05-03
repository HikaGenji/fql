\name{lj}
\alias{lj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lower join
}
\description{
The lj (left join) verb joins tables on the key columns of its right argument.
The left argument is a table, and the right argument is a keyed table,
where the key columns must be a subset of the columns of the left argument.
}
\usage{
lj(x, y, keep.y=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A dataframe.
}
  \item{y}{
A keyed dataframe. key attribute can be set by xkey function.
}
  \item{keep.y}{
  a boolean whether or not keeping non matching y records in the result
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
JFP
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{uj}} and \code{\link{xkey}}
}
\examples{
  x <- data.frame(a=c(1,2,3),
                  b=c("x", "y", "z"),
                  c=c(10, 20, 20)
  )
  y <- data.frame(a=c(1,3),
                  b=c("x", "z"),
                  c=c(1,2),
                  d=c(10,20))
  
  x \%>\% lj(y \%>\% xkey(c("a", "b")))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lj}
\keyword{join}% __ONLY ONE__ keyword per line
