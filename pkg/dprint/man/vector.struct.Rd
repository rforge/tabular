\name{vector.struct}
\alias{vector.struct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Structure
%%  ~~function to do ... ~~
}
\description{Defines the structure of a vector given to footnote, main
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vector.struct(vctr = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vctr}{
%%     ~~Describe \code{vctr} here~~
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
function(vctr=NA)
{
  vctr.nrw=NA
  if (!is.na(vctr[1]))
    {
      vctr      <- vector.linebreak(vctr)
      vctr.nrw  <- length(vctr)
    }
  return(list(vctr=vctr,          # Newly formatted title
              vctr.nrw=vctr.nrw   # Number of rows title will take
              ))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
