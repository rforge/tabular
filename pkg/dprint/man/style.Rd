\name{style}
\alias{style}
\title{Style Sheet}
\usage{
  style(frmt.bdy = NULL, frmt.col = NULL, frmt.colh = NULL,
    frmt.grp = NULL, frmt.lbl = NULL, frmt.main = NULL,
    frmt.tbl = NULL, frmt.ftn = NULL, justify = "center",
    indent = 2, tbl.buf = 0.25, cex = 1)
}
\arguments{
  \item{frmt.bdy}{format settings for body of table}

  \item{frmt.col}{format settings for column heading of
  table}

  \item{frmt.colh}{Only used for borders around column
  spannin in hiearchy, "o" boxes entrie column, "_" puts
  line under hiarchy}

  \item{frmt.grp}{format settings for row group labels}

  \item{frmt.lbl}{format settings for row labels}

  \item{frmt.main}{format settings for table title}

  \item{frmt.tbl}{format settings for entire table,
  currently only for the box around entire table (Except
  table title)}

  \item{frmt.ftn}{format settings for footnote}

  \item{justify}{justification of text, applies to column
  heading and body}

  \item{indent}{number of characters ("A") to indent the
  labels underneath the grouping variable}

  \item{tbl.buf}{The space (vertical) between multiple
  tables}

  \item{cex}{character expansion}
}
\description{
  Control the mark up and formats for different sections of
  table
}
\examples{
# My Style
CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
            frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_"),
            frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
            frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
            frmt.ftn=frmt(fontfamily="HersheySans"),
            justify="right")
}

