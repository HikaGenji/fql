\name{xprev}
\alias{xprev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return the nth previous element to each item in a vector
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xprev(l, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
A vector
}
  \item{k}{
An integer, the number of shift in index to perform
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector, lagged
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
  l <- 1:12
  l \%>\% xprev(-2)
  l \%>\% xprev(12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lag}
\keyword{xprev}% __ONLY ONE__ keyword per line
