#' Format of Table
#'
#' Creates a data structure to define the gpar settings passed to the various grid.* function calls, 
#' as well as other formatting controls
#'
#' @param fontfamily see gpar
#' @param fontface see gpar
#' @param fontsize see gpar
#' @param text.all If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row to avoid excessive parameters
#' @param col Color of text
#' @param bg Background Color
#' @param buf number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
#' @param bty style for box around section of table. "=" = above and below, "o" = completely enclosed, "_"=underneath
#' @param lwd  line size for bty, -1 suppresses
#' @param lty line type for bty
#' @param lcol color of lines
#' @param linespace Number of lines between columns
#' @export
frmt <-
function(fontfamily = "",
                 fontface = "plain",
                 fontsize = 8,
                 text.all=TRUE,      # If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row
                 col = "black",       # Color of text
                 bg = "white",        # Background Color
                 buf = 3,             # number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
                 bty = "=",           # style for border "=" - above and below, "o"- rectangle around table
                 lwd = 1,             # line size for rectangle
                 lty = 1,             # Line Size, -1 Suppresses
                 lcol="black",        # lcol, col for lines of rectange
                 linespace = 2        # linespace  = space between rows, number of characters according to GPAR.TBL, will be converted to metric by PAGE.LAYOUT
                     )
{
    return(list(fontfamily = fontfamily,  fontface = fontface,  fontsize = fontsize, col=col, bg=bg, buf=buf,
                bty = bty, lwd = lwd, lty=lty, lcol=lcol,
                 text.all=text.all,linespace=linespace))
}

