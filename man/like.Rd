\name{\%like\%}
\alias{\%like\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
evaluate regular expression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
x %like% pattern
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character vector
}
  \item{pattern}{
A regular expression
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of booleans
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
#sym <- c("MSFT", "IBM")
#sym %like% "^MS"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{like}
