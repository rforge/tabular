#' Footer
#'
#' Footer
#'
#' @param txt1
#' @param frmt1
#' @param page
#' @param date
#' @param pagelayout.obj
#' @param pgtxt2
#' @param pagenum
#' @export
ftr <-
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
    grid.text(format(Sys.Date(), "%m/%d/%Y"),
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

