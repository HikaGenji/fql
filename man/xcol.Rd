\name{xcol}
\alias{xcol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The verb xcol renames columns in a table. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xcol(d, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
A dataframe
}
  \item{cols}{
A character vector of new column names, of length less than or equal to the number of columns in the table
}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  d <- data.frame(n=c("x", "y", "x", "z", "z", "y"),
                  p=c(0,15,12,20,25,14),
                  stringsAsFactors=FALSE
  )
  d \%>\% xcol("m")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rename}
\keyword{col}% __ONLY ONE__ keyword per line
