\name{fitpage}
\alias{fitpage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit To Page
%%  ~~function to do ... ~~
}
\description{Calcuates character expansion to apply accross table presentation so that table fits within margins
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fitpage(size.simp, pagelayout, fit.width = FALSE, fit.height = FALSE, fit = FALSE, cex.0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{size.simp}{
%%     ~~Describe \code{size.simp} here~~
}
  \item{pagelayout}{
%%     ~~Describe \code{pagelayout} here~~
}
  \item{fit.width}{
%%     ~~Describe \code{fit.width} here~~
}
  \item{fit.height}{
%%     ~~Describe \code{fit.height} here~~
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{cex.0}{
%%     ~~Describe \code{cex.0} here~~
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
function(size.simp,
                    pagelayout,
                    fit.width=FALSE,
                    fit.height=FALSE,
                    fit=FALSE,
                    cex.0            # default cex
                    )
{
  cex.width  <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)]))/size.simp$tbl.width
  cex.height <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)]))/size.simp$tbl.height.mf
  cntrbuf.horz <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)])-size.simp$tbl.width)/2
  cntrbuf.vert <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)])-size.simp$tbl.height.mf)/2

  if (fit.width&fit.height|fit) {cex.fit<-min(cex.width, cex.height)}
  else if (fit.width)           {cex.fit<-cex.width}
  else if (fit.height)          {cex.fit<-cex.height}
  else                          {cex.fit<-cex.0}

  return(list(cex.fit=cex.fit,cntrbuf.horz=cntrbuf.horz, cntrbuf.vert=cntrbuf.vert))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
