\name{row.hl}
\alias{row.hl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Row Highlight
%%  ~~function to do ... ~~
}
\description{Creates a data structure for highlighting rows of the table based on indices provided by the user
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
row.hl(dx, col = "yellow")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dx}{
%%     ~~Describe \code{dx} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dx,
                   col="yellow")
  {
   if(is.logical(dx)) {dx<-which(dx)}
   return(list(dx=dx, col=col))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
