\name{conditional.struct}
\alias{conditional.struct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conditional Structure
%%  ~~function to do ... ~~
}
\description{For variables defined as conditional variables in the formula, creates a structure to drive creating a list of simple table structures
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
conditional.struct(data, byvars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{byvars}{
%%     ~~Describe \code{byvars} here~~
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
function(data, byvars)
  {
    cnms <- colnames(data)[!colnames(data) \%in\% byvars]
    if (length(byvars)==1)
      { concat.cond <- paste(rep(byvars, ncol(data)), data[,byvars], sep="=")}
    else
      {
        l <- vector("list", length(byvars))
        for (byvars.i in 1:length(byvars))
          {
            l[[byvars.i]] <- paste(rep(byvars[byvars.i], ncol(data)), data[, byvars[byvars.i]], sep="=")
          }

          concat.cond <- apply(data.frame(l), 1, paste, collapse=", ")
      }
    uniq.concat.cond <- unique(concat.cond)
    data <- data[, -which(colnames(data) \%in\% byvars)]
    if (!is.data.frame(data))
      {data <- as.data.frame(data); colnames(data) <- cnms; }
   return(list(concat.cond=concat.cond, uniq.concat.cond=uniq.concat.cond, data=data))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
