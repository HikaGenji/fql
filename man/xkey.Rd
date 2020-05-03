\name{xkey}
\alias{xkey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the primary keys in a table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xkey(d, keys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{keys}{
A character vector of column keys
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with a key attribute
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
\code{\link{lj}}
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
\keyword{xkey}

