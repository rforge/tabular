\name{dprint.table}
\alias{dprint.table}
\title{Print Table to Graphics Device (dprint)}
\usage{
  dprint.table(fmla = NULL, data, label = NULL,
    group = NULL, regx = NA, style = NULL, main = NA,
    footnote = NA, dtype = "rgraphics", pg.dim = NULL,
    margins = NULL, showmargins = FALSE, row.hl = NULL,
    fit.width = FALSE, fit.height = FALSE, fit = FALSE,
    newpage = FALSE, center.horz = FALSE,
    center.vert = FALSE, center = FALSE, f.hdr = NULL,
    f.ftr = NULL, pagenum = NULL, lastcall = NULL)
}
\arguments{
  \item{fmla}{Formula interface to define table structure.
  See fmla function}

  \item{data}{Input Data.frame}

  \item{label}{name of column containing row labels}

  \item{group}{name of column containing hieriarchy labels
  for the row names}

  \item{regx}{regular expression to be removed from
  original column names}

  \item{style}{style sheet object.  See style function}

  \item{main}{table title}

  \item{footnote}{footnote}

  \item{dtype}{Graphics device type referred to by names,
  sets default page settings.Device type, currently
  available "rdevice", "portrait", "landscape"}

  \item{pg.dim}{If custom page dimensions define a vector
  of c(width, height) units. Custom page dimensions over
  ride dtype}

  \item{margins}{A numerical vector of the form c(bottom,
  left, top, right) which gives the margin size specified
  in inches. Other forms include a constant for all margins
  and c(top/bottom,left/right)}

  \item{showmargins}{Display margins on page in red.
  Usefull for tinkering with presentation}

  \item{row.hl}{row hightlight object. Conditional
  highlight; row.hl(dx=, col=)}

  \item{fit.width}{logical. If TRUE, Forces the table to
  fit the table horizontally}

  \item{fit.height}{logical. If TRUE, Forces the table to
  fit the table vertically, if conditional variabled
  defined will force the first table to fit one page and
  same ratio will be applied to all tables}

  \item{fit}{logical. If TRUE, Force the table to fit both
  horizontally&vertically (smaller of two cex
  calulcations)}

  \item{newpage}{logical. If TRUE, when the device runs out
  of space a new page will automatically be started.
  DEsigned for multipage pdf reports.}

  \item{center.horz}{boolean, Center table horizontally}

  \item{center.vert}{boolean, Center table vertically,
  should only be used on one table}

  \item{center}{boolean, Center both vertically and
  horizontally. These should probably not be used with the
  fit.* parameters}

  \item{f.hdr}{Pass Function for printing header}

  \item{f.ftr}{Pass Function for printing Footer}

  \item{pagenum}{Starting page number, will override
  pagenumber from lastcall}

  \item{lastcall}{Last call from dprint}
}
\description{
  Print Table to Graphics Device. Manages page layout and
  prints multiple tables by dispatching DPRINT.SIMP (shold
  probably be a data.frame class of dprint)
}
\examples{
### Generate Sample Data Structures ###
# Generate some example data frames
table1   <- rdesc(4, 5)        # Numeric
table1f <- rdesc(4, 5, rnd=TRUE) # Rounded and pretty format so values are character
table1a <- table1; table1b<-table1;
table1a$group2 <- 1; table1b$group2 <- 2;
table2 <- rbind(table1a, table1b)
table2a <- table2; table2b<-table2
table2a$group3 <- "Zebra"; table2b$group3 <- "Elephant";
table3 <- rbind(table2a, table2b)
# Create style object
CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
            frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_"),
            frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
            frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
            frmt.ftn=frmt(fontfamily="HersheySans"),
            justify="right")

x11()# All variables, no group or label
dprint(~., data=table1f)
dev.off()
x11() # Spanning,  group level, and apply control and treatments to hierchaies on right
dprint(group+level~Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, data=table1f)
dev.off()
x11(); #Illegal Names, remove expression
dprint(group+level~`This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
dev.off()
x11(); #Illegal Names, no group label
dev.off()
dprint(~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1f, regx="1|2|.y")
x11(); # all on rhs with exception of p.value
dev.off()
dprint(group+level~.-p.value, table1f)

x11();
dprint(fmla=group+level~., data=table1)
dev.off()
x11()
dprint(fmla=group+level~Rn(round(Mean1, 2), "Mean Trt")+Rn(round(Variance1,2), "Variance"), data=table1)
dev.off()
x11()
dprint(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), table1, main="Dumb Ass")
dev.off()
x11()
dprint(level~.|group2, table2)
dev.off()
x11();
dprint(level~Mean1+Median2|group2, table2, main="Descriptives")
dev.off()
x11(); # Spanning, embedded fuctions, and conditional
dprint(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
dev.off()
x11(); # Spanning, embedded fuctions, and conditional
dev.off()
dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)
x11(); # Spanning, embedded fuctions, and conditional
dev.off()
dprint(~Treatment:(Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Rn(round(Median1,2), "Median"))|group2, table2)
dev.off()
x11()
dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+Control:Rn(paste(round(Mean2, 2),"(", round(Variance2, 2),")"), "Mean Trt (Std)")|group2, table2)
dev.off()

f1 <- group+level~Treatment:Rn(Fc(Mean1, Variance1), "Mean (Std)")+Control:Rn(Fc(Mean2, Variance2), "Mean (Std)") + Rn(round(p.value,2), "P-value")
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Center")
dev.off()
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Right", style=style(justify="right", frmt.tbl=frmt(bty="o")))
dev.off()
x11()
dprint(fmla=f1, data=table1,margins=.2, main="Justify Left",  style=style(justify="left", frmt.tbl=frmt(bty="o")))
dev.off()

h <- expression(hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5))))
f <- expression(ftr("VNSNY:  The Center for Home Care\\n               Policy & Research", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1.25, 1, 1.25,1))))
x11()
dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
            style=style(justify="left", frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(linespace=1.5, bty="X")),
            f.hdr = h, f.ftr=f, pagenum=1)
dev.off()

x11()
dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
            style=CBs,
            f.hdr = h, f.ftr=f, pagenum=1)
}

