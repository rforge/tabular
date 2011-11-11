\name{char.width1}
\alias{char.width1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Character Width
%%  ~~function to do ... ~~
}
\description{Returns the max width in units, given gpar settings
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
char.width1(vctr, frmt, cx = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vctr}{
%%     ~~Describe \code{vctr} here~~
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
function(vctr, frmt, cx=1)
{

   if (is.factor(vctr)) {vctr <- as.character(vctr)}

   n.char   <- nchar(vctr)
   cwidth.n <- max(n.char, na.rm=T)
   max.dx   <- which.max(cwidth.n)

   cwidth.u <- max(unlist(lapply(vctr,FUN=function(x) {convertWidth(grobWidth(textGrob(x, gp=gpar(fontfamily = frmt$fontfamily,
                                                                     fontsize = frmt$fontsize,
                                                                     fontface = frmt$fontface,
                                                                     cex = cx
                                                                      ))), "inches", valueOnly =TRUE)})), na.rm=TRUE)

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
