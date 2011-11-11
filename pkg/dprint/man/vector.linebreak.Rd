\name{vector.linebreak}
\alias{vector.linebreak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Line Break
%%  ~~function to do ... ~~
}
\description{To break vectors apart around an escape character [backslash]n that indicates a line break.  This is designed for handling line breaks for the table title (main) and the footnotes
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vector.linebreak(vctr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vctr}{
%%     ~~Describe \code{vctr} here~~
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
function(vctr)
{
  grep.linebreak <- grep("\n",vctr)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak   <- strsplit(vctr[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak          <- length(strsplit.linebreak[[1]]) # store how many line breaks
       v.temp               <- strsplit.linebreak[[1]]
       if (length(vctr)==1) {vctr <- v.temp}
       else
         {
           if (grep.linebreak[1] == 1)
            {  # Insert new expanded rows into orignal object (if occurs at begining of data frame)
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.temp, v.after)
            }
           else if (grep.linebreak[1] == length(vctr))
            {  # Insert new expanded rows into orignal object (if occurs at end of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              vctr      <- c(v.before, v.temp)
            }
           else
            {  # Insert new expanded rows into orignal object (if occurs in middle of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.before, v.temp, v.after)
            }
            grep.linebreak <- grep("\n",vctr)
            # Recursive Calls
            if (length(grep.linebreak) > 0) {vctr <- vector.linebreak(vctr)}
         }
    }
  vctr
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
