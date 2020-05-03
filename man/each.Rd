\name{each}
\alias{each}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply function to a list
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
each(l, fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
A list
}
  \item{fun}{
A function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list of evaluated function calls
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
each(list(1,2), function(x) 1+x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lapply}
