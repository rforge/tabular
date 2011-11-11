\name{dprint.lm}
\alias{dprint.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dprint.lm(lm.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.obj}{
%%     ~~Describe \code{lm.obj} here~~
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
function(lm.obj)
{
  tabular.obj <- as.tabular.lm(lm.obj)
  hh <- expression(hdr("Linear Model Summary", frmt1=frmt(fontfamily="", fontface="bold", fontsize=14, linespace=1, lwd=2),
                      pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(.3, .1)))
                  )
  dd <- dprint.table(param~., tabular.obj[[1]], main="Parameter Estimates",
                     style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")),
                     margins=c(.5, .25),
                     f.hdr=hh
                     )
  dprint.table(Stat~., data=tabular.obj[[2]], lastcall=dd, main="Model Summary Statistics",
               style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
