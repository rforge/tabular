\name{frmt}
\alias{frmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Format
%%  ~~function to do ... ~~
}
\description{Creates a data structure to define the gpar settings passed to the various grid.* function calls, as well as other formatting controls
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
frmt(fontfamily = "", fontface = "plain", fontsize = 8, text.all = TRUE, col = "black", bg = "white", buf = 3, bty = "=", lwd = 1, lty = 1, lcol = "black", linespace = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fontfamily}{
%%     ~~Describe \code{fontfamily} here~~
}
  \item{fontface}{
%%     ~~Describe \code{fontface} here~~
}
  \item{fontsize}{
%%     ~~Describe \code{fontsize} here~~
}
  \item{text.all}{If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row to avoid excessive parameters
%%     ~~Describe \code{text.all} here~~
}
  \item{col}{Color of text
%%     ~~Describe \code{col} here~~
}
  \item{bg}{Background Color
%%     ~~Describe \code{bg} here~~
}
  \item{buf}{number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
%%     ~~Describe \code{buf} here~~
}
  \item{bty}{style for box around section of table. "=" = above and below, "o" = completely enclosed, "_"=underneath
%%     ~~Describe \code{bty} here~~
}
  \item{lwd}{line size for bty, -1 suppresses
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type for bty
%%     ~~Describe \code{lty} here~~
}
  \item{lcol}{color of lines
%%     ~~Describe \code{lcol} here~~
}
  \item{linespace}{Number of lines between columns
%%     ~~Describe \code{linespace} here~~
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
\code{\link{gpar}}
\code{\link{style}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Default
frmt()
# Change, several options, others refer to default
frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_")
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(fontfamily = "",
                 fontface = "plain",
                 fontsize = 8,
                 text.all=TRUE,      # If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row
                 col = "black",       # Color of text
                 bg = "white",        # Background Color
                 buf = 3,             # number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
                 bty = "=",           # style for border "=" - above and below, "o"- rectangle around table
                 lwd = 1,             # line size for rectangle
                 lty = 1,             # Line Size, -1 Suppresses
                 lcol="black",        # lcol, col for lines of rectange
                 linespace = 2        # linespace  = space between rows, number of characters according to GPAR.TBL, will be converted to metric by PAGE.LAYOUT
                     )
{
    return(list(fontfamily = fontfamily,  fontface = fontface,  fontsize = fontsize, col=col, bg=bg, buf=buf,
                bty = bty, lwd = lwd, lty=lty, lcol=lcol,
                 text.all=text.all,linespace=linespace))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
