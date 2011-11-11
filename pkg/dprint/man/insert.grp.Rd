\name{insert.grp}
\alias{insert.grp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert Values Table Structure
%%  ~~function to do ... ~~
}
\description{Dispatcher for insert.grp1. Inserts NA positions for tbl, label, and group structure from tbl.obj.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
insert.grp(tbl.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tbl.obj}{
%%     ~~Describe \code{tbl.obj} here~~
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
\code{\link{insert.grp1}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(tbl.obj)
{
  if (!is.null(tbl.obj$group))
   {
      tbl.obj$bdy         <- insert.grp1(tbl.obj$bdy, tbl.obj$newgrp.dx, tbl.obj$lblescp)
      if (!is.null(tbl.obj$label))
        {tbl.obj$label  <- insert.grp1(tbl.obj$label, tbl.obj$newgrp.dx, tbl.obj$lblescp)}
      tbl.obj$group       <- insert.grp1(tbl.obj$group, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T)
      tbl.obj$newgroup    <- insert.grp1(tbl.obj$newgrp, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T, val=FALSE)
      tbl.obj$lastofgroup <- insert.grp1(tbl.obj$lastofgroup, tbl.obj$newgrp.dx, tbl.obj$lblescp, val=FALSE)
      tbl.obj$nrw         <- tbl.obj$nrw + length(tbl.obj$newgrp.dx) - sum(tbl.obj$lblescp)
      if(!is.null(tbl.obj$row.hl$dx)) # Row Highlight
        {tbl.obj$row.hl$dx <- insert.grp1(tbl.obj$row.hl$dx, tbl.obj$newgrp.dx, tbl.obj$lblescp, FALSE,val=1, dx.up=TRUE)}
      # This must go last
      {tbl.obj$newgrp.dx <- which(tbl.obj$newgroup)}
   }
 return(tbl.obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
