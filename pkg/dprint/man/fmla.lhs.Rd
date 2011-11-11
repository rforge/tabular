\name{fmla.lhs}
\alias{fmla.lhs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula Interface: Left Hand Side
%%  ~~function to do ... ~~
}
\description{Parses Left hand side of formula
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmla.lhs(f, grp = NULL, lvl = NULL, rhs.lpad = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{grp}{
%%     ~~Describe \code{grp} here~~
}
  \item{lvl}{
%%     ~~Describe \code{lvl} here~~
}
  \item{rhs.lpad}{
%%     ~~Describe \code{rhs.lpad} here~~
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
\code{\link{fmla.inter}}
\code{\link{fmla.rhs}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(f, grp=NULL, lvl=NULL, rhs.lpad=FALSE) {

  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0
  colon.op  = FALSE
  Rn.op     = FALSE
  lvl.found = FALSE

  if (l == 1) return(f)
  for (tm in fl) {
    i=i+1
    if (i==1) {
      rl[[i]] = tm
      if (tm == ":")  colon.op = TRUE
      if (tm == "Rn") Rn.op = TRUE
      if (tm == "|")  rl[[i]] = as.symbol("+")
    }
    if (i > 1) {
      if (typeof(tm) == "language")
           {rl[[i]] = fmla.lhs(tm)
           }
      else #group or level
           if (tm == lvl) lvl.found = TRUE
             rl[[i]] = tm
    }
  }
  if (colon.op == TRUE)  return(rl[[3]]) #third element is the variables that are spanned
  if (Rn.op == TRUE)     return(rl[[2]]) #second element is variables being renamed
  if (rhs.lpad == TRUE && lvl.found == TRUE) return(rl[[2]]) #second element is level
  return(as.call(rl))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
