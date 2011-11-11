\name{colnames.linebreak}
\alias{colnames.linebreak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Column Names Line Break
%%  ~~function to do ... ~~
}
\description{Inserts a line break in column names when the escape character [backslash] n is found.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colnames.linebreak(colnames.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colnames.obj}{
%%     ~~Describe \code{colnames.obj} here~~
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
function(colnames.obj  # Column Names Object creatd by COLNAMES.STRUCT
                              )
{
  grep.linebreak <- grep("\n",colnames.obj$cname)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak  <- strsplit(colnames.obj$cname[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak         <- length(strsplit.linebreak[[1]]) # store how many line breaks
       d.temp         <- colnames.obj[rep(grep.linebreak[1], n.linebreak), ] # Expand for each linebreak in this particular name
       d.temp$cname   <- strsplit.linebreak[[1]] # insert section of string that was delimitted by the string break character
       d.temp$col.row <- n.linebreak:1
       if (grep.linebreak[1] == 1)
        {  # Insert new expanded rows into orignal object (if row occurs at begining of data frame)
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind( d.temp, d.after)
        }
       else if (grep.linebreak[1] == nrow(colnames.obj))
        {  # Insert new expanded rows into orignal object (if row occurs at end of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          colnames.obj <- rbind(d.before,  d.temp)
        }
       else
        {  # Insert new expanded rows into orignal object (if row occurs in middle of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind(d.before,  d.temp, d.after)
        }
        grep.linebreak <- grep("\n",colnames.obj$cname)
        # Recursive Calls
        if (length(grep.linebreak) > 0) {colnames.obj <- colnames.linebreak(colnames.obj)}
    }
  colnames.obj
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
