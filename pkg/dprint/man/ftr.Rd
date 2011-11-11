\name{ftr}
\alias{ftr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ftr(txt1, frmt1 = frmt(fontfamily = "", fontface = "plain", fontsize = 8, col = "black", linespace = 0.75), page = TRUE, date = TRUE, pagelayout.obj = pagelayout(dtype = "portrait", margins = c(1, 0.5)), pgtxt2 = "page", pagenum = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt1}{
%%     ~~Describe \code{txt1} here~~
}
  \item{frmt1}{
%%     ~~Describe \code{frmt1} here~~
}
  \item{page}{
%%     ~~Describe \code{page} here~~
}
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{pagelayout.obj}{
%%     ~~Describe \code{pagelayout.obj} here~~
}
  \item{pgtxt2}{
%%     ~~Describe \code{pgtxt2} here~~
}
  \item{pagenum}{
%%     ~~Describe \code{pagenum} here~~
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
                frmt1=frmt(fontfamily="", fontface="plain", fontsize=8, col="black", linespace=.75),
                page=TRUE,
                date = TRUE,
                pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)), # Header is printed in reference to the margins of this function
                pgtxt2 = "page", # Text appended to page number
                pagenum=NULL
                  )
{

  txt1.struct <- vector.struct(txt1)
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  if (is.null(pagenum)) {print('test');pagenum <- eval.parent(pagenum, n = 1)}

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (date)
  {
    y.loc <- y.rem -  (linespace1)*txt1.dx
    grid.text(format(Sys.Date(), "\%m/\%d/\%Y"),
              just = "right",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (page)
  {
    y.loc <- y.rem -  2*(linespace1)
    grid.text(paste(pgtxt2, "\n", pagenum, sep=""),
              just = "center",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width/2, "inches"), y = unit(y.loc, "inches"))
  }

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
