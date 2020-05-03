\name{ungroup}
\alias{ungroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coerce a list of dataframes into a single dataframe
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ungroup(l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
A list of dataframe with same columns
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe, concatenating all the items of l
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
cross(1:3, 2:7, c) \%>\% ungroup
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uj}
\keyword{ungroup}% __ONLY ONE__ keyword per line
