\name{ftr}
\alias{ftr}
\title{Footer}
\usage{
  ftr(txt1,
    frmt1 = frmt(fontfamily = "", fontface = "plain", fontsize = 8, col = "black", linespace = 0.75),
    date = TRUE,
    pagelayout.obj = pagelayout(dtype = "portrait", margins = c(1, 0.5)),
    pgtxt2 = "page", pagenum = NULL)
}
\arguments{
  \item{txt1}{A vector of text to be places on bottom right
  of footer}

  \item{frmt1}{style sheet data type frmt for text on
  bottom left}

  \item{date}{Boolean, should today's date be placed in
  bottom center of page}

  \item{pagelayout.obj}{Tells footer what type of page
  dprint is working with.  Has separate margins to allow
  for extra space between table presentation}

  \item{pgtxt2}{Text to be appended to page number. To
  suppress numbering, make NULL}

  \item{pagenum}{Didn't want parameter here, I wanted
  pagenum to be passed down the calls stack
  "eval.parent(pagenum, 1)" here is the only way I could
  get to work ~ carlin}
}
\description{
  Footer
}
\examples{
longtable1 <- rdesc(15, 7)
longtable2 <- rdesc(7, 4)

h <- expression(hdr("Multiple Page Report",
                pagelayout.obj=pagelayout(dtype="landscape", margins=c(.75, .5))))
f <- expression(ftr("R Package tabulaR",
                pagelayout.obj=pagelayout(dtype="landscape", margins=c(.75, .5))
                , pagenum=eval.parent(pagenum, 1)
                ))

# setwd()
pdf("longtable1.pdf", height=8.5, width=11)


  dp <- dprint(fmla= group+level~ `This is a Control`:(Mean1 + Variance1) + Treatment:(Mean2 + Variance2)+p.value,
         data=longtable1, showmargins=TRUE, dtype="landscape",
         newpage=TRUE, pagenum=1, margins=1,
         f.hdr=h, f.ftr=f
         )
  dprint(fmla= group+level~ `This is a Control`:(Mean1 + Variance1) + Treatment:(Mean2 + Variance2)+p.value,
         data=longtable2, showmargins=TRUE, dtype="landscape",
         newpage=TRUE, lastcall=dp, # Pick up with page numbering
         margins=1,
         f.hdr=h, f.ftr=f
         )
dev.off()
}

