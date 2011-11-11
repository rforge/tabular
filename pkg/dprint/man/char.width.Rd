\name{char.width}
\alias{char.width}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{Returns the max width of all the elements in a vector in units, handles data types.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
char.width(obj, frmt, cx = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{frmt}{
%%     ~~Describe \code{frmt} here~~
}
  \item{cx}{
%%     ~~Describe \code{cx} here~~
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
function(obj, frmt, cx=1)
{
   cwidth.n=NULL; cwidth.u=NULL;
   if (is.data.frame(obj))  # If data.frame return a vector of widths
     {
       for (j in 1:ncol(obj))
          { cw <- char.width1(obj[,j], frmt=frmt, cx=cx)
            cwidth.n <- c(cwidth.n, cw$cwidth.n); cwidth.u <- c(cwidth.u, cw$cwidth.u);
          }
     }
   else # Otherwise simple call
     {
       cw <- char.width1(obj, frmt=frmt, cx=cx)
       cwidth.n <- cw$cwidth.n; cwidth.u <- cw$cwidth.u;
     }

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
