\name{style}
\alias{style}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Style Sheet
%%  ~~function to do ... ~~
}
\description{Control the mark up and formats for different sections of table
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
style(frmt.bdy = NULL, frmt.col = NULL, frmt.colh = NULL, frmt.grp = NULL, frmt.lbl = NULL, frmt.main = NULL, frmt.tbl = NULL, frmt.ftn = NULL, justify = "center", indent = 2, tbl.buf = 0.25, cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frmt.bdy}{format settings for body of table
%%     ~~Describe \code{frmt.bdy} here~~
}
  \item{frmt.col}{format settings for column heading of table
%%     ~~Describe \code{frmt.col} here~~
}
  \item{frmt.colh}{Only used for borders around column spannin in hiearchy, "o" boxes entrie column, "_" puts line under hiarchy
%%     ~~Describe \code{frmt.colh} here~~
}
  \item{frmt.grp}{format settings for row group labels
%%     ~~Describe \code{frmt.grp} here~~
}
  \item{frmt.lbl}{format settings for row labels
%%     ~~Describe \code{frmt.lbl} here~~
}
  \item{frmt.main}{format settings for table title
%%     ~~Describe \code{frmt.main} here~~
}
  \item{frmt.tbl}{format settings for entire table, currently only for the box around entire table (Except table title)
%%     ~~Describe \code{frmt.tbl} here~~
}
  \item{frmt.ftn}{format settings for footnote
%%     ~~Describe \code{frmt.ftn} here~~
}
  \item{justify}{justification of text, applies to column heading and body
%%     ~~Describe \code{justify} here~~
}
  \item{indent}{number of characters ("A") to indent the labels underneath the grouping variable
%%     ~~Describe \code{indent} here~~
}
  \item{tbl.buf}{The space (vertical) between multiple tables
%%     ~~Describe \code{tbl.buf} here~~
}
  \item{cex}{character expansion
%%     ~~Describe \code{cex} here~~
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
\author{Carlin
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{frmt}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
style()
# My Style
CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
             frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_"),
             frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
             frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
             frmt.ftn=frmt(fontfamily="HersheySans"),
             justify="right")
             
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(frmt.bdy=NULL,
                  frmt.col=NULL,
                  frmt.colh=NULL, # For borders around column hiearchy, "o" boxes entrie column, "_" puts line under hiarchy
                  frmt.grp=NULL,
                  frmt.lbl=NULL,
                  frmt.main=NULL,
                  frmt.tbl=NULL,  # Currently only controls border around the entire table
                  frmt.ftn=NULL,
                  justify = "center", # justification of the elements of table (does not include labels or groups), default "center"
                  indent = 2,         # number of characters ("A") to indent the labels underneath the grouping variable
                  tbl.buf=.25,        # The space (vertical) between multiple tables
                  cex = 1             # cex        = character expansion
                 )
{
   if (is.null(frmt.bdy))  {frmt.bdy =frmt(fontfamily="", fontface="plain", fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="X", lwd=1, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.col))  {frmt.col =frmt(fontfamily="", fontface="bold",  fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="_", lwd=2, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.colh)) {frmt.colh=frmt(fontfamily="", fontface="plain", fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="_", lwd=1, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.grp))  {frmt.grp =frmt(fontfamily="", fontface="bold",  fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="=", lwd=1, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.lbl))  {frmt.lbl =frmt(fontfamily="", fontface="plain", fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="=", lwd=1, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.tbl))  {frmt.tbl =frmt(fontfamily="", fontface="plain", fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="=", lwd=1, lty=1, lcol="black", linespace=2)}
   if (is.null(frmt.main)) {frmt.main=frmt(fontfamily="", fontface="bold",  fontsize=10, text.all=TRUE, col="black", bg="white", buf=3, bty="=", lwd=1, lty=1, lcol="black", linespace=1.5)}
   if (is.null(frmt.ftn))  {frmt.ftn =frmt(fontfamily="", fontface="plain", fontsize=8,  text.all=TRUE, col="black", bg="white", buf=3, bty="=", lwd=1, lty=1, lcol="black", linespace=1.5)}
    # When TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.grp
   if (frmt.bdy$text.all)
    {
      frmt.col$fontfamily=frmt.bdy$fontfamily;                                      frmt.col$fontsize=frmt.bdy$fontsize;
      frmt.grp$fontfamily=frmt.bdy$fontfamily;                                      frmt.grp$col=frmt.bdy$col;
      frmt.lbl$fontfamily=frmt.bdy$fontfamily; frmt.lbl$fontface=frmt.bdy$fontface; frmt.lbl$fontsize=frmt.bdy$fontsize; frmt.lbl$col=frmt.bdy$col;
    }

   ### HANLDE Defaults ### when atleast one parameter is defind
#   if (!(frmt.bdy$bty \%in\% c("o", "=", "_", "-"))) {frmt.bdy$bty <- "X"} # When body param

   return(list(frmt.bdy=frmt.bdy, frmt.col=frmt.col, frmt.grp=frmt.grp, frmt.lbl=frmt.lbl, frmt.main=frmt.main, frmt.tbl=frmt.tbl, frmt.colh=frmt.colh, frmt.ftn=frmt.ftn,
               justify = justify, indent = indent, tbl.buf=tbl.buf,
                cex = cex))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
