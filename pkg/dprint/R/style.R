style <-
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
#   if (!(frmt.bdy$bty %in% c("o", "=", "_", "-"))) {frmt.bdy$bty <- "X"} # When body param

   return(list(frmt.bdy=frmt.bdy, frmt.col=frmt.col, frmt.grp=frmt.grp, frmt.lbl=frmt.lbl, frmt.main=frmt.main, frmt.tbl=frmt.tbl, frmt.colh=frmt.colh, frmt.ftn=frmt.ftn,
               justify = justify, indent = indent, tbl.buf=tbl.buf,
                cex = cex))
}

