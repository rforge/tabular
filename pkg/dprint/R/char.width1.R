#' Character Width
#'
#' Returns the max width in units, given gpar settings
#' 
#' @param vctr vector
#' @param frmt format object
#' @param cx shinkage parameter
#' @export
char.width1 <-
function(vctr, frmt, cx=1)
{

   if (is.factor(vctr)) {vctr <- as.character(vctr)}

   n.char   <- nchar(vctr)
   cwidth.n <- max(n.char, na.rm=T)
   max.dx   <- which.max(cwidth.n)

   cwidth.u <- max(unlist(lapply(vctr,FUN=function(x) {convertWidth(grobWidth(textGrob(x, gp=gpar(fontfamily = frmt$fontfamily,
                                                                     fontsize = frmt$fontsize,
                                                                     fontface = frmt$fontface,
                                                                     cex = cx
                                                                      ))), "inches", valueOnly =TRUE)})), na.rm=TRUE)

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}

