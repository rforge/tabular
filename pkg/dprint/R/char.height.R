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

