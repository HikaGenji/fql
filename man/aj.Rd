\name{aj}
\alias{aj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
As of join
}
\description{
The functions aj joins tables along columns that are usually time columns. 
}
\usage{
aj(by, x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{by}{
A vector of character representing the keys on which to perform the join. last element is typically time.
}
  \item{x}{
A dataframe
}
  \item{y}{
A dataframe to lookup value in.
}
}
\details{
aj(c(c1,...,cn),t1,t2)
where (c1,...,cn) is a list of common column names, and t1 and t2 are the tables to be joined.
The columns need not be keys on t1, and t2 must not have a key. 
Moreover common columns in t1 and t2 must be of the same type 
and cn (last sym in common columns) must be a common column, 
typically time, although aj can work on any other type if sorted. 

The result is a table with records from the left join of t1 and t2. 
 
For each record in t1, the result has one record with the items in t1, and 
if there are matching records in t2, the items of the last (in row order) matching record are appended to those of t1 
if there is no matching record in t2, the remaining columns are null 
}
\value{
A dataframe with values of y as of x taken on specified keys.
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
\code{\link{lj}} and \code{\link{uj}} 
}
\examples{
 x <- data.frame(time=c("10:01:01","10:01:03", "10:01:04"),
                  sym=c("msft", "ibm", "ge"),
                  qty=c(100,200,150))
  y <- data.frame(time=c("10:01:00", "10:01:00", "10:01:00", "10:01:02"),
                  sym=c("ibm", "msft", "msft", "ibm"),
                  px=c(100,99,101,98))
  by <- c("sym", "time")
  aj(by, x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{asof}
\keyword{join}% __ONLY ONE__ keyword per line
