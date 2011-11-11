\name{list.to.df}
\alias{list.to.df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List to Data.frame
%%  ~~function to do ... ~~
}
\description{Convert a list of vectors, of varying length, to a data frame. Elements are NA where lengths of vectors are smaller than the length of the largest vector.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
list.to.df(lst)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lst}{
%%     ~~Describe \code{lst} here~~
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
function(lst # this is a list returned from string split
                                  )
{
  lst.df <- NULL
  if (sum(unlist(lapply(lst, FUN=function(x) {length(x)}))) > 0)
  {
    mx <- max(unlist(lapply(lst, FUN=function(x) {length(x)})))
    for (i in 1:length(lst))
    {
     lst[[i]] <- c(lst[[i]], rep(NA, mx-length(lst[[i]])))
    }
    lst.df <- t(as.data.frame(lst, stringsAsFactors =F))
    colnames(lst.df) <- paste("C", 1:ncol(lst.df), sep="")
    rownames(lst.df) <- paste("R", 1:nrow(lst.df), sep="")
  }
  lst.df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
