\name{uj}
\alias{uj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Upper join
}
\description{
The uj verb is a type of union join that generalizes comma.
The result has the union of the columns filled with nulls where necessary. 
}
\usage{
uj(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A dataframe
}
  \item{y}{
A dataframe
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe
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
  x <- data.frame(a=c(1,2),
                  b=c(2,3),
                  c=c(5,7)
  )
  y <- data.frame(a=1:3,
                  b=c(2,3,7),
                  c=c(10,20,30),
                  d=c("A" , "B", "C")
  )
  
  # unkeyed, so second table is inserted
  x \%>\% uj(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{uj}
\keyword{join}% __ONLY ONE__ keyword per line
