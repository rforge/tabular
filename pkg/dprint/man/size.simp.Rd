\name{size.simp}
\alias{size.simp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Size of Simple Table
%%  ~~function to do ... ~~
}
\description{Measures size of properties of the simple table
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
size.simp(tbl.obj, char.dim.obj, pagelayout, loc.y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tbl.obj}{
%%     ~~Describe \code{tbl.obj} here~~
}
  \item{char.dim.obj}{
%%     ~~Describe \code{char.dim.obj} here~~
}
  \item{pagelayout}{
%%     ~~Describe \code{pagelayout} here~~
}
  \item{loc.y}{
%%     ~~Describe \code{loc.y} here~~
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
function(tbl.obj, char.dim.obj, pagelayout, loc.y)
{
  # see if label and group were defined for number of columns to allocate: want to be 0 or 1
  nlbl  <- min(1, length(tbl.obj$label))
  ngrp  <- min(1, length(tbl.obj$group))
  nmain <- max(c(0, tbl.obj$main.nrw), na.rm=T)
  nfoot <- max(c(0, tbl.obj$footnote.nrw+.5), na.rm=T)
  main.height     <- nmain*char.dim.obj$linespace.main
  footnote.height <- nfoot*char.dim.obj$linespace.ftn

  # To calculate the height of the current table Number of rows for each section times character height
  tbl.height    <- tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.m  <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.mf <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy + footnote.height
  tbl.width     <- char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[length(char.dim.obj$bdy.xloc)]

  line.height     <- c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.bdy, tbl.obj$nrw) + rep(char.dim.obj$linespace.footnote, nfoot))
  cumheight.bdy <- cumsum(rep(char.dim.obj$linespace.bdy, tbl.obj$nrw)) + sum(c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.footnote, nfoot)))

  fitspage <- cumheight.bdy <= (loc.y - pagelayout$margins[3])
  newpage=FALSE;
  tablebreak.dx = NULL;
  nothingfits <- FALSE

  if (sum(fitspage) < tbl.obj$nrw) # table will not fit on page
    {
      if (sum(fitspage)==0) {nothingfits <- TRUE}
      else if (!is.null(tbl.obj$group[1]))  # If grouping was defined keep groups together when possible
        {
          fitspage.grp.dx <- which(fitspage & tbl.obj$lastofgroup)
          if      (length(fitspage.grp.dx) > 0)
            {
             tablebreak.dx <- fitspage.grp.dx[length(fitspage.grp.dx)];
             newpage=TRUE
            }
          #else if (call2) {tablebreak.dx <- which(fitspage)[1]; newpage=TRUE}  #  After page is resetM, IF group is too, large break rule
          else  {nothingfits <- TRUE} #  Reset page and see if group works
        }
      else # Assume no group and fit rows to page
        {
          tablebreak.dx <- which(fitspage)[length(which(fitspage))]
          newpage <- TRUE
        }
      }

  return(list(tbl.height=tbl.height,           # table height (EXlcuding main/footnote)
              tbl.height.m=tbl.height.m,       # table height Including footnote
              tbl.height.mf=tbl.height.mf,     # table height Including main and footnote
              tbl.width=tbl.width,
              line.height=line.height,         # The height of each line in a vector,
              cumheight.bdy=cumheight.bdy,     # The height of each line in a vector
              nothingfits=nothingfits,
              tablebreak.dx=tablebreak.dx      # Rownumber for table to be split upon
              ))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
