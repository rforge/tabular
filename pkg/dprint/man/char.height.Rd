\name{char.height}
\alias{char.height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Character Height
%%  ~~function to do ... ~~
}
\description{Returns height in units(inches) based on a gpar setting
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
char.height(charact = "A", frmt, cx = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{charact}{
%%     ~~Describe \code{charact} here~~
}
  \item{frmt}{
%%     ~~Describe \code{frmt} here~~
}
  \item{cx}{
%%     ~~Describe \code{cx} here~~
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
function(charact = "A", frmt, cx=1)
{

  cheight.u <- convertHeight(grobHeight(textGrob(charact, gp=gpar(fontfamily = frmt$fontfamily,
                                                                  fontsize = frmt$fontsize,
                                                                  fontface = frmt$fontface,
                                                                  cex = cx
                                                                  ))), "inches", valueOnly =TRUE)
   return(cheight.u)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
