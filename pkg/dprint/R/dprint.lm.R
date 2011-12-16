#' Print Summary Of lm object
#'
#' dprint lm class (to be implemented)
#' 
#' @param lm.obj lm object
#' @export
dprint.lm <-
function(lm.obj)
{
  tabular.obj <- as.tabular.lm(lm.obj)
  hh <- expression(hdr("Linear Model Summary", frmt1=frmt(fontfamily="", fontface="bold", fontsize=14, linespace=1, lwd=2),
                      pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(.3, .1)))
                  )
  dd <- dprint.table(param~., tabular.obj[[1]], main="Parameter Estimates",
                     style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")),
                     margins=c(.5, .25),
                     f.hdr=hh
                     )
  dprint.table(Stat~., data=tabular.obj[[2]], lastcall=dd, main="Model Summary Statistics",
               style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")))
}

