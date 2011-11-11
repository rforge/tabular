\name{dprint.table}
\alias{dprint.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dprint.table
%%  ~~function to do ... ~~
}
\description{Print Table to Graphics Device. Manages page layout and prints multiple tables by dispatching DPRINT.SIMP
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dprint.table(fmla = NULL, data, label = NULL, group = NULL, regx = NA, style = NULL, main = NA, footnote = NA, dtype = "rgraphics", pg.dim = NULL, margins = NULL, showmargins = FALSE, row.hl = NULL, fit.width = FALSE, fit.height = FALSE, fit = FALSE, newpage = FALSE, center.horz = FALSE, center.vert = FALSE, center = FALSE, f.hdr = NULL, f.ftr = NULL, pagenum = NULL, lastcall = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{Formula interface to define table structure. See fmla function
%%     ~~Describe \code{fmla} here~~
}
  \item{data}{Input Data.frame
%%     ~~Describe \code{data} here~~
}
  \item{label}{name of column containing row labels
%%     ~~Describe \code{label} here~~
}
  \item{group}{name of column containing hieriarchy labels for the row names
%%     ~~Describe \code{group} here~~
}
  \item{regx}{regular expression to be removed from original column names
%%     ~~Describe \code{regx} here~~
}
  \item{style}{style sheet object.  See style function
%%     ~~Describe \code{style} here~~
}
  \item{main}{table title
%%     ~~Describe \code{main} here~~
}
  \item{footnote}{footnote
%%     ~~Describe \code{footnote} here~~
}
  \item{dtype}{Graphics device type referred to by names, sets default page settings.Device type, currently available "rdevice", "portrait", "landscape"
%%     ~~Describe \code{dtype} here~~
}
  \item{pg.dim}{If custom page dimensions define a vector of c(width, height) units. Custom page dimensions over ride dtype
%%     ~~Describe \code{pg.dim} here~~
}
  \item{margins}{A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches. Other forms include a constant for all margins and c(top/bottom,left/right)
%%     ~~Describe \code{margins} here~~
}
  \item{showmargins}{Display margins on page in red. Usefull for tinkering with presentation
%%     ~~Describe \code{showmargins} here~~
}
  \item{row.hl}{row hightlight object. Conditional highlight; row.hl(dx=, col=)
%%     ~~Describe \code{row.hl} here~~
}
  \item{fit.width}{logical. If TRUE, Forces the table to fit the table horizontally
%%     ~~Describe \code{fit.width} here~~
}
  \item{fit.height}{logical. If TRUE, Forces the table to fit the table vertically, if conditional variabled defined will force the first table to fit one page and same ratio will be applied to all tables
%%     ~~Describe \code{fit.height} here~~
}
  \item{fit}{logical. If TRUE, Force the table to fit both horizontally&vertically (smaller of two cex calulcations)
%%     ~~Describe \code{fit} here~~
}
  \item{newpage}{logical. If TRUE, when the device runs out of space a new page will automatically be started. DEsigned for multipage pdf reports.
%%     ~~Describe \code{newpage} here~~
}
  \item{center.horz}{Center table horizontally
%%     ~~Describe \code{center.horz} here~~
}
  \item{center.vert}{Center table vertically, should only be used on one table
%%     ~~Describe \code{center.vert} here~~
}
  \item{center}{Center both vertically and horizontally. These should probably not be used with the fit.* parameters
%%     ~~Describe \code{center} here~~
}
  \item{f.hdr}{Pass Function for printing header
%%     ~~Describe \code{f.hdr} here~~
}
  \item{f.ftr}{Pass Function for printing Footer
%%     ~~Describe \code{f.ftr} here~~
}
  \item{pagenum}{Starting page number, will override pagenumber from lastcall
%%     ~~Describe \code{pagenum} here~~
}
  \item{lastcall}{Last call from dprint
%%     ~~Describe \code{lastcall} here~~
}
}
\details{
fit parameters only work with vector fonts.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fmla}}
\code{\link{frmt}}
\code{\link{style}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
             frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_"),
             frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
             frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
             frmt.ftn=frmt(fontfamily="HersheySans"),
             justify="right")

x11()# All variables, no group or label
dprint(~., data=table1f)
x11() # Spanning,  group level, and apply control and treatments to hierchaies on right
dprint(group+level~Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, data=table1f)
x11(); #Illegal Names, remove expression
dprint(group+level~`This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
x11(); #Illegal Names, no group label
dprint(~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
x11(); # all on rhs with exception of p.value
dprint(group+level~.-p.value, table1f)

x11();
dprint(fmla=group+level~., data=table1)
x11()
dprint(fmla=group+level~Rn(round(Mean1, 2), "Mean Trt")+Rn(round(Variance1,2), "Variance"), data=table1)
x11()
dprint(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), table1, main="Dumb Ass")
x11()
dprint(level~.|group2, table2)
x11();
dprint(level~Mean1+Median2|group2, table2, main="Descriptives")
x11(); # Spanning, embedded fuctions, and conditional
dprint(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
x11(); # Spanning, embedded fuctions, and conditional
dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
x11(); # Spanning, embedded fuctions, and conditional
dprint(~Treatment:(Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Rn(round(Median1,2), "Median"))|group2, table2)
x11()
dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Control:Rn(paste(round(Mean2, 2),"(", round(Variance2, 2),")"), "Mean Trt (Std)")|group2, table2)


f1 <- group+level~Treatment:Rn(Fc(Mean1, Variance1), "Mean (Std)")+Control:Rn(Fc(Mean2, Variance2), "Mean (Std)") + Rn(round(p.value,2), "P-value")
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Center")
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Right", style=style(justify="right", frmt.tbl=frmt(bty="o")))
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Left",  style=style(justify="left", frmt.tbl=frmt(bty="o")))

h <- expression(hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5))))
f <- expression(ftr("VNSNY:  The Center for Home Care\n               Policy & Research", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1.25, 1, 1.25,1))))
x11()
dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=T, main="Table Left",
             style=style(justify="left", frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(linespace=1.5, bty="X")),
             f.hdr = h, f.ftr=f, pagenum=1)

 x11()
dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=T, main="Table Left",
             style=CBs,
             f.hdr = h, f.ftr=f, pagenum=1)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
              if (dtype \%in\% c("rgraphics")) { x11() }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
