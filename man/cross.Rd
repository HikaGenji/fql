\name{cross}
\alias{cross}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply a function on the cartesian product of two lists
}
\description{
Apply a function on the cartesian product of two lists
}
\usage{
cross(x, y, FUN = function(x, y) c(x, y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a list or vector
}
  \item{y}{
a list or vector
}
  \item{FUN}{
A binary function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of computed values for each item of the cartesian product of x and y
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
}
\examples{
  cross(1:3, 2:7)
  cross(1:3, 2:7, c) 
  cross(1:3, 2:7, function(x,y)x+y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cross}
\keyword{cartesian}% __ONLY ONE__ keyword per line
