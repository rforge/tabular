#' Print Table to Graphics Device (dprint)
#'
#' Print Table to Graphics Device. 
#' Manages page layout and prints multiple tables by dispatching DPRINT.SIMP
#' (shold probably be a data.frame class of dprint)
#' 
#' @param fmla Formula interface to define table structure. See fmla function
#' @param data Input Data.frame
#' @param label name of column containing row labels
#' @param group name of column containing hieriarchy labels for the row names
#' @param regx regular expression to be removed from original column names
#' @param  style style sheet object.  See style function
#' @param main table title
#' @param footnote footnote
#' @param dtype Graphics device type referred to by names, sets default page settings.Device type, currently available "rdevice", "portrait", "landscape"
#' @param pg.dim If custom page dimensions define a vector of c(width, height) units. Custom page dimensions over ride dtype
#' @param margins A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches. Other forms include a constant for all margins and c(top/bottom,left/right)
#' @param showmargins Display margins on page in red. Usefull for tinkering with presentation
#' @param row.hl row hightlight object. Conditional highlight; row.hl(dx=, col=)
#' @param fit.width logical. If TRUE, Forces the table to fit the table horizontally
#' @param fit.height logical. If TRUE, Forces the table to fit the table vertically, if conditional variabled defined will force the first table to fit one page and same ratio will be applied to all tables
#' @param fit logical. If TRUE, Force the table to fit both horizontally&vertically (smaller of two cex calulcations)
#' @param newpage logical. If TRUE, when the device runs out of space a new page will automatically be started. DEsigned for multipage pdf reports.
#' @param center.horz boolean, Center table horizontally
#' @param center.vert boolean, Center table vertically, should only be used on one table
#' @param center boolean, Center both vertically and horizontally. These should probably not be used with the fit.* parameters
#' @param f.hdr Pass Function for printing header
#' @param f.ftr Pass Function for printing Footer
#' @param pagenum Starting page number, will override pagenumber from lastcall
#' @param lastcall Last call from dprint
#' @export
#' @examples
#' ### Generate Sample Data Structures ###
#' # Generate some example data frames
#' table1   <- rdesc(4, 5)        # Numeric
#' table1f <- rdesc(4, 5, rnd=TRUE) # Rounded and pretty format so values are character
#' table1a <- table1; table1b<-table1;
#' table1a$group2 <- 1; table1b$group2 <- 2;
#' table2 <- rbind(table1a, table1b)
#' table2a <- table2; table2b<-table2
#' table2a$group3 <- "Zebra"; table2b$group3 <- "Elephant";
#' table3 <- rbind(table2a, table2b)
#' # Create style object
#' CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
#'             frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_"),
#'             frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
#'             frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
#'             frmt.ftn=frmt(fontfamily="HersheySans"),
#'             justify="right")
#'
#' x11()# All variables, no group or label
#' dprint(~., data=table1f)
#' dev.off()
#' x11() # Spanning,  group level, and apply control and treatments to hierchaies on right
#' dprint(group+level~Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, data=table1f)
#' dev.off()
#' x11(); #Illegal Names, remove expression
#' dprint(group+level~`This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
#' dev.off()
#' x11(); #Illegal Names, no group label
#' dev.off()
#' dprint(~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
#' x11(); # all on rhs with exception of p.value
#' dev.off()
#' dprint(group+level~.-p.value, table1f)
#'
#' x11();
#' dprint(fmla=group+level~., data=table1)
#' dev.off()
#' x11()
#' dprint(fmla=group+level~Rn(round(Mean1, 2), "Mean Trt")+Rn(round(Variance1,2), "Variance"), data=table1)
#' dev.off()
#' x11()
#' dprint(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), table1, main="Dumb Ass")
#' dev.off()
#' x11()
#' dprint(level~.|group2, table2)
#' dev.off()
#' x11();
#' dprint(level~Mean1+Median2|group2, table2, main="Descriptives")
#' dev.off()
#' x11(); # Spanning, embedded fuctions, and conditional
#' dprint(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
#' dev.off()
#' x11(); # Spanning, embedded fuctions, and conditional
#' dev.off()
#' dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
#' x11(); # Spanning, embedded fuctions, and conditional
#' dev.off()
#' dprint(~Treatment:(Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Rn(round(Median1,2), "Median"))|group2, table2)
#' dev.off()
#' x11()
#' dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Control:Rn(paste(round(Mean2, 2),"(", round(Variance2, 2),")"), "Mean Trt (Std)")|group2, table2)
#' dev.off()
#'
#' f1 <- group+level~Treatment:Rn(Fc(Mean1, Variance1), "Mean (Std)")+Control:Rn(Fc(Mean2, Variance2), "Mean (Std)") + Rn(round(p.value,2), "P-value")
#' x11()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Center")
#' dev.off()
#' x11()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Right", style=style(justify="right", frmt.tbl=frmt(bty="o")))
#' dev.off()
#' x11()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Left",  style=style(justify="left", frmt.tbl=frmt(bty="o")))
#' dev.off()
#'
#' h <- expression(hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5))))
#' f <- expression(ftr("VNSNY:  The Center for Home Care\n               Policy & Research", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1.25, 1, 1.25,1))))
#' x11()
#' dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
#'             style=style(justify="left", frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(linespace=1.5, bty="X")),
#'             f.hdr = h, f.ftr=f, pagenum=1)
#' dev.off()
#'
#' x11()
#' dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
#'             style=CBs,
#'             f.hdr = h, f.ftr=f, pagenum=1)
# @author Carlin Brickner
dprint.table <-
function(fmla=NULL,         # Formula interface to define table structure
         data,              # Input Data.frame
         label = NULL,      # label & group are characters identifying columns that define the simple table structure
         group = NULL,
         regx=NA,           # Regular Expression to take off of colnames, designed to break unwanted tiebreakers for legal data.frame columnnames
         style=NULL,        # List of graphical parameters or table format options returned from STYLE(),
         main=NA,           # Table Title, Vector of Strings
         footnote=NA,       # Footnote, Vector of Strings
         dtype="rgraphics", # Device type, currently available "rdevice", "portrait", "landscape"
         pg.dim=NULL,       # If custom page dimensions define a vector of c(width, height) units. Custom page dimensions over ride dtype
         margins=NULL,      # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
         showmargins=FALSE, # Display margins on page in red
         row.hl=NULL,       # Conditional highlight; row.hl(dx=, col=)
         fit.width=FALSE,   # Forces the table to fit the table horizontally
         fit.height=FALSE,  # Forces the table to fit the table vertically, if conditional variabled defined will force the first table to fit one page and same ratio will be applied to all tables
         fit=FALSE,         # Force the table to fit both horizontally&vertically (smaller of two cex calulcations)
         newpage=FALSE,     # when the device runs out of space, when TRUE a new page will automatically be started
         center.horz=FALSE, # Center table horizontally.
         center.vert=FALSE, # Center table vertically, should only be used on one table
         center=FALSE,      # Center both vertically and horizontally. These should probably not be used with the fit.* parameters
         f.hdr=NULL,        # Function for printing header
         f.ftr=NULL,        # Function for printing Footer
         pagenum=NULL,      # Starting page number, will override pagenumber from lastcall
         lastcall=NULL      # Last call from
                       )
{
  # Define Default formats for table if nothing defined
  if (is.null(style)) {style.obj <- style()}
  else (style.obj <- style)

  # Define page layout objects
  pglay.obj <- pagelayout(dtype=dtype, pg.dim=pg.dim, margins=margins)

  # Define table structure from formula interface
  tbl.obj <- tbl.struct(fmla=fmla, data=data, label=label, group=group, regx=regx, main=main, footnote=footnote, row.hl=row.hl)

  # Insert records and position structures in table object for group formatting.  Must be done before the page management calcs.
  # This inserts NA's when a new group appears to have a horizontal line break between groups.  This should have a format swicth to turn off and on
  for (tbl.i in 1:length(tbl.obj)) {tbl.obj[[tbl.i]] <- insert.grp(tbl.obj[[tbl.i]])}

  # Initialize parameters for looping through many sub tables
  if (is.null(lastcall))
    { d <-NULL; d$cord1 <- d$cord2 <- pglay.obj$cord.tl;
      pagenum <- ifelse(is.null(pagenum), 1, pagenum);  # If pagenumber is not defined assume first page
    }
  else
    {d <- lastcall;
     pagenum <- ifelse(is.null(pagenum), d$pagenum, pagenum);  # If pagenumber is not defined assume first page
    }
  tablesexist <- TRUE; tbl.i <- 1;

  while (tablesexist) # lOOP through a list of table structure until complete, list may become longer because of page breaks
    {
    if (tbl.i==1)
      { # Calculate character expansion ratios (cex) to fit table exactly to margins (results in 1 otherwise)
        fitpage.obj <- fitpage(size.simp(tbl.obj[[1]], char.dim(tbl.obj, style=style.obj, cx=style.obj$cex), pglay.obj, pglay.obj$cord.tl[2]),
                               pglay.obj, fit.width=fit.width, fit.height=fit.height, fit=fit, style.obj$cex)
        style.obj$cex <- fitpage.obj$cex.fit # Assign calculated cex fit, defaults to 1 or user defined
        cntrbuf.horz <- ifelse(center.horz|center, fitpage.obj$cntrbuf.horz, 0)
        cntrbuf.vert <- ifelse(center.vert|center, fitpage.obj$cntrbuf.vert, 0)
        if (!is.null(f.hdr)){eval(f.hdr)}
        if (!is.null(f.ftr)){eval(f.ftr)}
        pagenum <- pagenum+1
      }
      # Insert the distance between tables after first table has been printed
      tbl.buf <- ifelse((tbl.i==1) & (is.null(lastcall)) , 0, style.obj$tbl.buf)
      # Update y.pos
      y.pos <- d$cord2[2] - tbl.buf - cntrbuf.vert
      x.pos <- d$cord1[1] + cntrbuf.horz

      # Character dimensions for page and table layout calculations
      char.dim.obj  <- char.dim(tbl.obj, style=style.obj, cx=fitpage.obj$cex.fit)
      size.simp.obj <- size.simp(tbl.obj[[tbl.i]], char.dim.obj, pglay.obj, y.pos)

      if (newpage)
        {
          # If nothing in table fits than start a newpage
          if (size.simp.obj$nothingfits)
            {
              y.pos <- pglay.obj$cord.tl[2] # Reset y.pos to the top of the page
              if (dtype %in% c("rgraphics")) { x11() }
              else { grid.newpage() }
              if (!is.null(f.hdr)){eval(f.hdr)}
              if (!is.null(f.ftr)){eval(f.ftr)}
              # Measure up page once again
              size.simp.obj <- size.simp(tbl.obj[[tbl.i]], char.dim.obj, pglay.obj, y.pos)
            }
          if (!is.null(size.simp.obj$tablebreak.dx)) # Break the table on the last line or group that fits on the page and continue
            {tbl.obj <- tablebreak(tbl.obj, tbl.i, size.simp.obj$tablebreak.dx);
             # Measure up page once again, after breaking up the current table
             size.simp.obj <- size.simp(tbl.obj[[tbl.i]], char.dim.obj, pglay.obj, y.pos)
            }
        }
      d <- dprint.simp(tbl.obj[[tbl.i]], init=c(x.pos, y.pos),
                       style=style.obj, char.dim.obj=char.dim.obj, size.simp.obj=size.simp.obj)

      # Display margins on page
      if(showmargins) {dborder(pglay.obj$cord.tl, pglay.obj$cord.br, frmt=frmt(bty="o", lcol="red"))}
      tbl.i <- tbl.i+1;
      if (tbl.i > length(tbl.obj)) {tablesexist<- FALSE}
    }
    d$pagenum <- pagenum
    return(d)
}

