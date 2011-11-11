\name{kill.multiregx}
\alias{kill.multiregx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kill Multiple Regular Expression
%%  ~~function to do ... ~~
}
\description{Compacting some regular expression logic
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kill.multiregx(string, regx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
%%     ~~Describe \code{string} here~~
}
  \item{regx}{
%%     ~~Describe \code{regx} here~~
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
function(string, # string
                           regx # Perl Regular expression
                           )
{

  string <- sub(regx, '', string, perl = TRUE)
  g.spc <- grep(regx, string)
  if(length(g.spc) > 0)   { string <- kill.multiregx(string, regx) }
  return(string)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
