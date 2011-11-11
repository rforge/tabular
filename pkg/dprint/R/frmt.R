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

