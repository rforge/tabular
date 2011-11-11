\name{tbl.struct}
\alias{tbl.struct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Table Structure
%%  ~~function to do ... ~~
}
\description{Generalization of table structure
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tbl.struct(fmla = NULL, data, label = NULL, group = NULL, regx = NA, main = NA, footnote = NA, row.hl = list(dx = NULL, col = NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{Formula interface to define table structure
%%     ~~Describe \code{fmla} here~~
}
  \item{data}{data.frame
%%     ~~Describe \code{data} here~~
}
  \item{label}{name of column containing row labels
%%     ~~Describe \code{label} here~~
}
  \item{group}{name of column containing hieriarchy labels for the row names
%%     ~~Describe \code{group} here~~
}
  \item{regx}{regular expression to be removed from original column names
%%     ~~Describe \code{regx} here~~
}
  \item{main}{Table title
%%     ~~Describe \code{main} here~~
}
  \item{footnote}{footnote
%%     ~~Describe \code{footnote} here~~
}
  \item{row.hl}{row highlight object see row.hl function
%%     ~~Describe \code{row.hl} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list of one or more simple table structure
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
function(fmla=NULL,    # Formula interface to define table structure
                       data,         # Input Data.frame
                       label = NULL, # label & group are characters identifying columns that define the simple table structure
                       group = NULL,
                       regx=NA,      # Regular Expression to take off of colnames, designed to break unwanted tiebreakers for legal data.frame columnnames
                       main=NA,      # Table Title, Vector of strings where each element is a new line
                       footnote=NA,  # Footnote, Vector of strings where each element is a new line
                       row.hl=list(dx=NULL, col=NULL) #  Conditional Formatting to highlight rows
                       )
{
  tbl.obj <- vector("list", 1)
  ### Parameter Declaration ###
  if(is.null(fmla))
    {
      tbl.obj[[1]] <- tbl.struct.simp(data=data, label = label, group = group, main=main, footnote=footnote)
      # Conditional Formatting
      tbl.obj[[1]]$row.hl <- row.hl  # Row Highlight
  }
  ### Formula Interface ###
  else
    {
      fmla.obj   <- fmla.inter(fmla, data=data, regx=regx)
      # If no conidionalt variables than simple table structure
      if (is.null(fmla.obj$byvars1))
        {
          tbl.obj[[1]]  <- tbl.struct.simp(data=fmla.obj$tbl, label = fmla.obj$label, group = fmla.obj$group, main=main, footnote=footnote, colnames.obj=fmla.obj$colnames.obj)
          # Conditional Formatting
          tbl.obj[[1]]$row.hl <- row.hl  # Row Highlight
        }
      ### Condional Variables Used ###
      else # create a list of simple table structures by all combinations of values of conditional variables
        {
          conditional.obj     <- conditional.struct(fmla.obj$tbl, byvars=fmla.obj$byvars1)
           l.uniq.concat.cond  <- length(conditional.obj$uniq.concat.cond)
           tbl.obj             <- vector("list", l.uniq.concat.cond)
           data <- conditional.obj$data # Removes conditional variables

           for (uniq.concat.cond.i in 1:l.uniq.concat.cond)
            {
               cur.fltr.dx <- which(conditional.obj$concat.cond == conditional.obj$uniq.concat.cond[uniq.concat.cond.i])
               data.i <- data[cur.fltr.dx, ]
               if (!is.data.frame(data.i)) {data.i <- as.data.frame(data.i)} # Class change on subsetting nx1 data frame
               tbl.obj[[uniq.concat.cond.i]] <- tbl.struct.simp(data=data.i, label = fmla.obj$label, group = fmla.obj$group, main=main, footnote=footnote, colnames.obj=fmla.obj$colnames.obj)
               tbl.obj[[uniq.concat.cond.i]]$cond.txt <- conditional.obj$uniq.concat.cond[uniq.concat.cond.i]
               ### Conditional Formatting ###
               # Row highlight
               if (!is.null(row.hl$dx))
               {
                  tbl.obj[[uniq.concat.cond.i]]$row.hl <-list(dx=NULL, col=NULL)
                  row.hl.dx <- which(row.hl$dx <= max(cur.fltr.dx))
                  tbl.obj[[uniq.concat.cond.i]]$row.hl$dx  <- row.hl$dx[row.hl.dx]
                  tbl.obj[[uniq.concat.cond.i]]$row.hl$col <- row.hl$col
                  row.hl$dx <- row.hl$dx[-row.hl.dx]-nrow(data.i)
               }
            }
        }
    }
  tbl.obj
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
