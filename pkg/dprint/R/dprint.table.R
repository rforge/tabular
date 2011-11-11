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

