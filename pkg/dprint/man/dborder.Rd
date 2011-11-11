\name{dborder}
\alias{dborder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Border
%%  ~~function to do ... ~~
}
\description{Draws a border style arround a section of table
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dborder(cord1, cord2, frmt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cord1}{
%%     ~~Describe \code{cord1} here~~
}
  \item{cord2}{
%%     ~~Describe \code{cord2} here~~
}
  \item{frmt}{
%%     ~~Describe \code{frmt} here~~
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
function(cord1,  # Vector (x,y) indicating position of top left point of rectangle
                  cord2,  # Vector (x,y) indicating position of top left point of rectangle
                  frmt    #
                 )
{
  ### Prints a box - suppresse lines based on symbol ###
  if (frmt$bty \%in\% c("=", "o", "-"))
  {# TOP Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord1[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty \%in\% c("=","o", "_"))
  { # BOTTOM Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord2[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }

  if (frmt$bty \%in\% c("o"))
  {# LEFT Line
    grid.lines( x = unit(cord1[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty \%in\% c("o"))
  {# RIGHT Line
    grid.lines( x = unit(cord2[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
