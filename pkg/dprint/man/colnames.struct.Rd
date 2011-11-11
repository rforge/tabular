\name{colnames.struct}
\alias{colnames.struct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Column Names Structure
%%  ~~function to do ... ~~
}
\description{Creates a structure for printing column names and their hierachies
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colnames.struct(col.names, linebreak = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{linebreak}{
%%     ~~Describe \code{linebreak} here~~
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
function(col.names, linebreak=TRUE)
{
  # look for escape character indicating that a grouping scheme exists for column names
  col.grp.dx      <- grep(":", col.names)
  col.grp.split   <- strsplit(col.names, ":")
  orig.names      <- unlist(lapply(col.grp.split, FUN=function(x) {x[length(x)]}))
  lst.names       <- lapply(col.grp.split, FUN=function(x) {x[-length(x)]}) # group names (if any) after removing origninal column names
  lnames          <- length(orig.names)
  # Logical row keeps column hiercahies together, col.row indicates whether line breaks have been performed
  column.heading  <- data.frame(cname = orig.names, col.logical.row = rep(1, lnames), col.row=1, span.beg=1:lnames, span.end=1:lnames, stringsAsFactors =F)
  r.i             <- nrow(column.heading)
  cname.hierc.df  <- list.to.df(lst.names)

  if(!is.null(cname.hierc.df))
    {
      for (hier.i in ncol(cname.hierc.df):1)
        {
          d.i <- consect.struct(cname.hierc.df[, hier.i])
          n.d <- nrow(d.i$consec.begend)
          column.heading[r.i+ (1:n.d), "col.logical.row"] <- ncol(cname.hierc.df)-hier.i+2
          column.heading[r.i+ (1:n.d), "col.row"] <-1
          column.heading[r.i+(1:n.d), c("cname", "span.beg", "span.end")] <- d.i$consec.begend
          r.i <- nrow(column.heading)
        }
    }
  column.heading$cname  <- kill.multiregx(column.heading$cname, "`") #  Remove backhashes
  if (linebreak) # Can be called in two different places with different assumptions whether the line break has been added yet
    {
      column.heading        <- colnames.linebreak(column.heading)        #  Account for line breaks in column names
      column.heading        <- colnames.row(column.heading)              #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
    }
  column.heading
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
