\name{Format}
\alias{Fc}
\alias{Fb}
\alias{Fci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inline Formula Formatting of Columns
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fc(mn, std, digits = 2, nsmall = 2, NAmiss = TRUE)
Fb(prop = NULL, r = NULL, n = NULL, digits.p = 4, nsmall.p = 2, digits.n = 6)
Fci(cil, ciu, mn = NA, digits = 2, nsmall = 2, NAmiss = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mn}{column name reprentign a mean
%%     ~~Describe \code{mn} here~~
}
  \item{std}{column name reprentign a standard deviation (Variance, Margin of Error, etc.)
%%     ~~Describe \code{std} here~~
}
  \item{digits}{see format
%%     ~~Describe \code{digits} here~~
}
  \item{nsmall}{see format
%%     ~~Describe \code{nsmall} here~~
}
  \item{NAmiss}{
%%     ~~Describe \code{NAmiss} here~~
}
  \item{prop}{column name representing a proportion
%%     ~~Describe \code{prop} here~~
}
  \item{r}{column name representing number of binomial events observed
%%     ~~Describe \code{r} here~~
}
  \item{n}{column name representing number of observations
%%     ~~Describe \code{n} here~~
}
  \item{digits.p}{digits for proportion presentation. see format
%%     ~~Describe \code{digits.p} here~~
}
  \item{nsmall.p}{nsmall for proportion presentation.
%%     ~~Describe \code{nsmall.p} here~~
}
  \item{digits.n}{number of digits for n presentation
%%     ~~Describe \code{digits.n} here~~
}
  \item{cil}{column name representing lower bound of interval
%%     ~~Describe \code{cil} here~~
}
  \item{ciu}{column name representing upper bound of interval
%%     ~~Describe \code{ciu} here~~
}
}
\details{
Fc formats data representing summaries continuous data where the results is of the form 'mn(sd)'
Fb formats data representing summaries of binomia data where the result is of the form 'xx.xx \% (r)'
Fci formats data representing data representing lower and upper bound of an interval where the results are of the form 'mn (cil, ciu)'
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
\code{\link{format}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(mn, std, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(std, digits), digits=digits, nsmall=nsmall), ")", sep="")
  if (NAmiss) { f[is.na(mn)] <- ""}
  f
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
