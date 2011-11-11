\name{colnames.row}
\alias{colnames.row}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Column Names Row
%%  ~~function to do ... ~~
}
\description{Adjust index (reference number of rows above table for colnames) to account for line breaks
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colnames.row(colnames.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colnames.obj}{
%%     ~~Describe \code{colnames.obj} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(colnames.obj)
  {
     max.row <- tapply(colnames.obj$col.row, list(colnames.obj$col.logical.row), function(x) max(x, na.rm=T))
     d.max.row <- data.frame(max.row)
     d.max.row$col.logical.row <- rownames(d.max.row)
     d.max.row$lag.max.row.adj <- c(0, d.max.row$max.row[-nrow(d.max.row)]-1)
     colnames.obj <- merge(colnames.obj, d.max.row, by ="col.logical.row")
     colnames.obj$row <- colnames.obj$col.logical.row + colnames.obj$col.row -1 + colnames.obj$lag.max.row.adj
     colnames.obj$row <- max(colnames.obj$row) - (colnames.obj$row-1)
     colnames.obj
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
