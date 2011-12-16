#' Character Height
#'
#' Returns height in units(inches) based on a gpar setting
#' 
#' @param charact Table Object
#' @param frmt format object
#' @param cx shinkage parameter
#' @export
char.height <-
function(charact = "A", frmt, cx=1)
{

  cheight.u <- convertHeight(grobHeight(textGrob(charact, gp=gpar(fontfamily = frmt$fontfamily,
                                                                  fontsize = frmt$fontsize,
                                                                  fontface = frmt$fontface,
                                                                  cex = cx
                                                                  ))), "inches", valueOnly =TRUE)
   return(cheight.u)
}

