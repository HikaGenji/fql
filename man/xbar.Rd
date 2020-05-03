\name{xbar}
\alias{xbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
evaluate regular expression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Partition a vector by buckets
}
\usage{
y %xbar% x
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector
}
  \item{y}{
A scalar
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of indices
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
# time  <- sample(seq(times("09:00:01"), times("09:59:59"), by=times("00:00:01")), 100, replace=FALSE) %>% asc
# bucket<- 5 %xbar% minutes(time)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{xbar}
