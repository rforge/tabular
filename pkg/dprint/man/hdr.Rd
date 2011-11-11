\name{hdr}
\alias{hdr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hdr(txt1, frmt1 = frmt(fontfamily = "", fontface = "bold", fontsize = 20, col = "blue", bty = "_", lwd = 2, linespace = 1), txt2 = NA, frmt2 = frmt(fontfamily = "", fontface = "bold", fontsize = 16, col = "red", bty = "_", lwd = 2, linespace = 1), pagelayout.obj = pagelayout(dtype = "portrait", margins = c(1, 0.5)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt1}{
%%     ~~Describe \code{txt1} here~~
}
  \item{frmt1}{
%%     ~~Describe \code{frmt1} here~~
}
  \item{txt2}{
%%     ~~Describe \code{txt2} here~~
}
  \item{frmt2}{
%%     ~~Describe \code{frmt2} here~~
}
  \item{pagelayout.obj}{
%%     ~~Describe \code{pagelayout.obj} here~~
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
function(txt1,
                frmt1=frmt(fontfamily="", fontface="bold", fontsize=20, col="blue", bty="_", lwd=2, linespace=1),
                txt2=NA, # oPTIONAL, see as being used for indicating CONFIDENTIAL document justified right, in upper right hand corner
                frmt2=frmt(fontfamily="", fontface="bold", fontsize=16, col="red", bty="_", lwd=2, linespace=1),
                pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)) # Header is printed in reference to the margins of this function
                )
{

  txt1.struct <- vector.struct(txt1)
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  y.rem <- y.loc <- pagelayout.obj$page.height
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }
  grid.lines(x = unit(c(pagelayout.obj$margins[2], pagelayout.obj$page.width-pagelayout.obj$margins[4]), "inches"),
             y = unit(pagelayout.obj$page.height-pagelayout.obj$margins[3], "inches"),
             gp = gpar(col = frmt1$lcol, fontsize = frmt1$fontsize, cex = frmt1$cex, lwd = frmt1$lwd, lty=frmt1$lty),
                draw = TRUE)

  if (!is.na(txt2))
  {
    txt2.struct <- vector.struct(txt2)
    ch2         <- char.height("A", frmt=frmt2, cx=1)
    linespace2  <- frmt2$linespace*ch2
    y.loc <- y.rem
    for (txt2.dx in 1:txt2.struct$vctr.nrw)
      {
        y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace2)*txt2.dx
        grid.text(txt2.struct$vctr[txt2.dx],
                  just = "right",
                  gp = gpar(fontfamily = frmt2$fontfamily, fontface = frmt2$fontface, fontsize = frmt2$fontsize, cex = frmt2$cex, col = frmt2$col),
                  x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))

      }
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
