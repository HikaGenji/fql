\name{fills}
\alias{fills}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fills forward missing values in a vector
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fills(object, na.rm = FALSE, maxgap = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A vector
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{maxgap}{
%%     ~~Describe \code{maxgap} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector with missing values propagated forward
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fills(c(1, NA, 2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fills}
\keyword{locf}% __ONLY ONE__ keyword per line
