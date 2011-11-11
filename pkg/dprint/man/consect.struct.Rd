\name{consect.struct}
\alias{consect.struct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Consecutive Structure
%%  ~~function to do ... ~~
}
\description{Given a vector returns several objects describing how(if) consecutive elements appear.  This is a helper function for tbl.struct and is important in defining the column hierarchy.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
consect.struct(vct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vct}{
%%     ~~Describe \code{vct} here~~
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
\author{Carlin
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tbl.struct}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(vct  # vector
                          )
{
  temp.vct <- vct
  lvct <- length(vct) #length of vector
  # Handle NAs
  temp.vct[is.na(vct)] <- paste("NA.", 10000+(1:length(which(is.na(vct)))), sep="") # NA's should be treated as distinct
  next.vct  <- c(temp.vct[2:lvct], NA) # the next position
  # Indicate when the next consective value begins
  new.vct       <- temp.vct != next.vct
  new.vct       <- c(TRUE, new.vct[-lvct])
  new.vct.dx    <- which(new.vct)
  new.vct.gdx   <- cumsum(new.vct)
  consec.info   <- data.frame(vct, temp.vct, next.vct, new.vct, new.vct.gdx, stringsAsFactors =F)
  if (lvct>1) # Exception handler for when just vector is of length 1
  {
    min.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {min(x)})
    max.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {max(x)})
  }
  else {min.dx=1; max.dx=1}
  consec.begend <- data.frame(uniq.val=vct[new.vct], mn=min.dx, mx=max.dx, stringsAsFactors =F)
  return(list(consec.info=consec.info, consec.begend=consec.begend[!is.na(consec.begend$uniq.val), ]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
