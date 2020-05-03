\name{\%^\%}
\alias{\%^\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replace missing value with value
}
\description{

}
\usage{
value \%^\% table
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
a vector or a dataframe
}
  \item{value}{
a value
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A Dataframe
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
  0 \%^\% c(1, NA, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{coalesce}