\name{insert.grp1}
\alias{insert.grp1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert Values
%%  ~~function to do ... ~~
}
\description{Inserts a value into record in tbl.struct objects to control for when empty rows should be inserted into the data structure.  NA's will be suppressed during presentation of the table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
insert.grp1(obj, dx, lblescp, group = F, val = NA, dx.up = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{dx}{
%%     ~~Describe \code{dx} here~~
}
  \item{lblescp}{
%%     ~~Describe \code{lblescp} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{dx.up}{
%%     ~~Describe \code{dx.up} here~~
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
function(obj,      # vector or data frame
                      dx,         # Index where NA is to be inserted
                      lblescp,    # Label Escape, boolean
                      group=F,  # indicates that this object is related to the grouping vector and the insetions must go after
                      val=NA,
                      dx.up =FALSE # If true adjust an index
)
{
  # This section handles data frames
  if (is.data.frame(obj))
    {
      for (i in 1:length(dx))
        {
          if (!lblescp[i])
            { if(i==1)  {obj <- rbind(rep(val, ncol(obj)), obj)}
              else
                { # Exception to handle object changing class when subsetting
                  data1 <- obj[1:(dx[i]-1), ]
                  data2 <- obj[(dx[i]):nrow(obj), ]
                  if (!is.data.frame(data1)) {data1 <- as.data.frame(data1); colnames(data1) <- "a1";}
                  if (!is.data.frame(data2)) {data2 <- as.data.frame(data2); colnames(data2) <- "a1";}
                   obj <- rbind(data1, rep(val, ncol(obj)), data2)
                }
              dx <- dx + 1
            }
        }
    }
  else if(dx.up)
    { obj2 <- obj
      for (dx.i in 1:length(dx))
        {
          if (!lblescp[i])
            {          obj2[obj>=dx[dx.i]] <- obj2[obj>=dx[dx.i]] +1}
        }
      obj<-obj2
    }
  # This section handles vectors
  else
    { # This section handles vectors with the exception of groups
      if (!group)
        {
          for (i in 1:length(dx))
            {
              if (!lblescp[i])
               {
                  if(i==1) {obj <- c(val, obj)}
                  else {obj <- c(obj[1:(dx[i]-1)], val, obj[(dx[i]):length(obj)])}
                  dx <- dx + 1
                }
            }
        }
      else # This section handles the group vector, index is slightly altered to put NA after the group name
        {
          for (i in 1:length(dx))
            {
              if (!lblescp[i])
               {
                  if(i==length(dx)) {obj <- c(obj, val)}
                  else {obj <- c(obj[1:(dx[i+1]-1)], val, obj[(dx[i+1]):length(obj)])}
                  dx <- dx + 1
               }
            }
        }
    }
  return(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
