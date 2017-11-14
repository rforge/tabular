
### COMPILE SOURCE ###
#setwd()
source("dprint.r")


# ============== #
#       RDESC    #
# ============== #

### Generate Sample Data Structures ###
# Generate some example data frames for debuging
table1   <- rdesc(4, 5)        # Numeric
table1f <- rdesc(4, 5, rnd=T) # Rounded and pretty format so values are character
table1a <- table1; table1b<-table1;
table1a$group2 <- 1; table1b$group2 <- 2;
table2 <- rbind(table1a, table1b)
table2a <- table2; table2b<-table2

table2a$group3 <- "Zebra"; table2b$group3 <- "Elephant";
table3 <- rbind(table2a, table2b)

# ===================== #
#   FORMULA INTERFACE   #
# ===================== #
fmla_inter(~., table1)            # print everything in data frame
fmla_inter(level~., table1)       # level and everything
fmla_inter(group+level~., table1) # row dimensions exist identfied by group & level,  and everything else is printed
fmla_inter(~Mean1 + Median1 + Variance1, table1) # No group or level + subset of fields on the right
fmla_inter(group+.~ Mean1 + Median1 + Variance1, table1)     #group, but no level, and subet of fields on right
fmla_inter(group+level~ Mean1+Median1, table1)
fmla_inter(group+level~ Mean1 + Median1 + Variance1, table1) # group and level, and subset of fields on right

### Spanning ###
fmla_inter(group+level~ Control:(Mean1 + Median1 + Variance1), table1) # group level, and apply Control as hierachy to fields on right
fmla_inter(group+level~ Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, table1) # group level, and apply control and treatments to hierchaies on right
fmla_inter(group+level~ Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, table1) # group level, and apply control and treatments to hierchaies on right
# Illegal name for formula, and use regualr expression to take of text.  This will be useful to prevent erros when tie brekers
fmla_inter(group+level~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1, regx="1|2|.y")
fmla_inter(group+level~.-p.value, table1) # all on rhs with exception of p.value

fmla_inter(~.-p.value, table1)   # Columns but p.value

### Condition logic ###
# Condition on group2
fmla_inter(group+level~ Mean1 + Median1 + Variance1|group2, table2)
# Condition on group2 and group3
fmla_inter(group+level~ Mean1 + Median1 + Variance1|group3+group2, table3)
# Condition on group2 down, and group3 accross page (not yet implemented in presentation)
fmla_inter(group+level~ Mean1 + Median1 + Variance1|group2|group3, table3)
fmla_inter(group+level~ Mean1 + Median1 + Variance1|.|group3, table3) # Just across

fmla_inter(level~.|group2, table2)
fmla_inter(~.|group2, table2)
fmla_inter(group+.~.|group2, table2)
fmla_inter(group+.~.|.|group3, table3)
fmla_inter(group+level~.|group2, table2)

fmla_inter(group+level~x:(Mean1+Median1)+y:(Mean2+Median2)|group2, table2)

### Rename and other imbedded functions ##
fmla_inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla_inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), table1)
fmla_inter(group+level~Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)"), table1)
# Spanning + Embedded
fmla_inter(group~Treatment:Rn(round(Mean1, 2), "Mean Trt"), table1)
fmla_inter(group~Treatment:Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla_inter(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)"), table1)
fmla_inter(group+level~Control:(Mean1 + Median1 + Variance1), table1)
fmla_inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla_inter(group+level~Rn(round(Mean1, 2), "Mean\nTrt")+Variance1, table1)
fmla_inter(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)



# =================== #
#    CONSECT.STRUCT   #
# =================== #
A  <- c("A", "A", "A", "B", "B", "B", "A", "A", "C")
# NA's should be treated as unique values
A1 <- c(NA, "A", "A", "A", "B", "B", "B", "A", "A", "C")
A2 <- c(NA, "A", "A", "A", "B", "B", "B", NA, NA, NA, "A", "A", "C")
consect.struct(A)
consect.struct(A1)
consect.struct(A2)
B <- c("Control2", "Control2", "Control2", "Treatment", "Treatment", "Treatment")
B1 <- c("Control1", "Control1", NA, NA,NA, NA)
consect.struct(B)
consect.struct(B1)

# FORMAT and STYLE #
# Default
frmt()
# Change, several options, others refer to default
frmt(fontfamily="HersheySans", bg="khaki", fontface="bold", lwd=2, bty="_")

# DEfault
style()
# My Style
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
dprint(group+level~Rn(Fr(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), data=table1, main="Don't Do this")
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
dprint(fmla=f1, data=table1,margins=.2, main="Justify Center", style=style(frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(lty=2, lcol="light blue"), frmt.col=frmt(bg="khaki")))
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


by_var_f1 <- level~Mean1+Median1|group
by_var_f2 <- level~Mean1+Median1|group+group2
fmla_inter(f1, data=table2)
tbl.struct(fmla=f1, data=table2, label=NULL, group=NULL, regx=NA, main=" A", footnote=NA, row.hl=NULL)
conditional.struct(table2, c("group2"))
conditional.struct(table2, c("group", "group2"))
# If main is default (null) than do not print titles
dprint(fmla=by_var_f1, data=table2)
# WHen title is defined, and only one conditional variable is defined, just print the values concatenated to the text
dprint(fmla=by_var_f1, data=table2,main=" ")
# When more than one conditional variable, concatenate the variable name and the current combination of values
dprint(fmla=by_var_f2, data=table2,main="Descriptives for: ")


# -------------------------------------------------------------------------------------------------------#
# Below here is old
# VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV #

### COLNAMES.STRUCT ###
# Creates example column names for debugging of COLNAMES.STRUCT
(cc <- col


names(fmla.obj$tbl))



(c1 <- colnames(fmla.obj1$tbl)[-c(1,2)])
(c2 <- colnames(fmla.obj2$tbl)[-c(1,2)])
(c3 <- colnames(fmla.obj3$tbl)[-c(1,2)])
(c3b<- colnames(fmla.obj3b$tbl)[-c(1,2)])
(c3c<- colnames(fmla.obj3c$tbl)[-c(1,2)])

colnames.struct(cc)
colnames.struct(c1)
colnames.struct(c2)
colnames.struct(c3)
colnames.struct(c3b)
colnames.struct(c3c)

c3.z <- c3
c3.z[1] <- "`This\nis a\nControl`:Me\nan"
colnames.struct(c3.z)

(t.fact1    <- terms(f1, data=table1, keep.order=T))
(t.trmlbls1 <- attr(t.fact1, "term.labels"))

### HIERC.RENAME ###
hierc.rename(table1, t.trmlbls1, regx=".y")

# This had an error, debuggin
(t.fact2    <- terms(group+level~., data=table2, keep.order=T))
(t.trmlbls2 <- attr(t.fact2, "term.labels"))
hierc.rename(table2, t.trmlbls2)

 strsplit(t.trmlbls1, ":")






### VECTOR.LINEBREAK  ###
vector.linebreak(c("A\nA"))
vector.linebreak(c("A\nA", "B"))
vector.linebreak(c("A\nA", "B", "C\nC\nC"))

vector.struct(c("Simple Title"))
vector.struct(c("Title with \n Two Lines"))
vector.struct(c("Title\nwith \n many\n Lines"))

### TBL.STRUCT ###
# Simple table
(a <- tbl.struct.simp(fmla.obj$tbl, fmla.obj$label, fmla.obj$group))
(a1 <- tbl.struct.simp(fmla.obj1$tbl, fmla.obj1$label, fmla.obj1$group, main="Simple Title"))
(a2 <- tbl.struct.simp(fmla.obj2$tbl, fmla.obj2$label, fmla.obj2$group, main="Title\nwith \n many\n Lines"))
(a3 <- tbl.struct.simp(fmla.obj3$tbl, fmla.obj3$label, fmla.obj3$group, footnote="This table sucks"))
(a3c <- tbl.struct.simp(fmla.obj3b$tbl, fmla.obj3c$label, fmla.obj3c$group))
(a3b <- tbl.struct.simp(fmla.obj3c$tbl, fmla.obj3b$label, fmla.obj3b$group))
s1 <- tbl.struct.simp(fmla.obj.s1$tbl, fmla.obj.s1$label, fmla.obj.s1$group)

### insert.grp ###
aa <- NULL
(aa$tbl   <- insert.grp1(a2$bdy, a2$newgrp.dx))
(aa$label <- insert.grp1(a2$label, a2$newgrp.dx))
(aa$group <- insert.grp1(a2$group, a2$newgrp.dx, group=T))
(aa$newgroup <- insert.grp1(a2$newgrp, a2$newgrp.dx, group=T, FALSE))
(aa$lastofgroup <- insert.grp1(a2$lastofgroup, a2$newgrp.dx, group=F, FALSE))
insert.grp1(which(table1$p.value<0.05), a$newgrp.dx, group=F, 1, dx.up=T)


# These two are equivalent
(a <- tbl.struct(data=table1))
(a2 <- tbl.struct(data=table1, label="level", group= "group"))
(a <- tbl.struct(fmla=f1, data=table1, footnote="This table sucks"))
(a0 <- tbl.struct(fmla=f0, data=table1))
(a <- tbl.struct(fmla=f1, data=table1, regx="1|2|.y"))

(a0  <- tbl.struct(fmla=f0, data=table1, main="Test Title"))
(a1  <- tbl.struct(fmla=f1, data=table1))
(a2  <- tbl.struct(fmla=f2, data=table1))
(a3  <- tbl.struct(fmla=f3, data=table1))
(a3b <- tbl.struct(fmla=f3b, data=table1))
(a3c <- tbl.struct(fmla=f3c, data=table1))

(a1  <- tbl.struct(fmla=f1, data=table1, row.hl=row.hl(1:3)))
insert.grp(a1[[1]])


### CONDITIONAL.STRUCT ###
conditional.struct(table2, c("group2"))
conditional.struct(table3, c("group3", "group2"))

(ac2 <- tbl.struct(fmla=fc2, data=table2))
(ac3 <- tbl.struct(fmla=fc3, data=table3))

#(a <- tbl.struct(data=table2, label=fmla.obj.s1$label, group=fmla.obj.s1$group))
#(a <- tbl.struct(data=fmla.obj.s1$tbl, label=fmla.obj.s1$label, group=fmla.obj.s1$group, row.hl=row.hl(2, "black")))

### CHAR.WIDTH1 ###
char.width1(a3c[[1]]$label, frmt=frmt())

char.width(a3c[[1]]$label, frmt=frmt())
char.width(a3c[[1]]$group, frmt=frmt())
char.width(a3c[[1]]$bdy, frmt=frmt())



### CHARACT.HEIGHT ###
# Returns height in units(inches) based on a gpar setting
char.height("A", frmt=frmt())

### DPRINT.SIMP ###
dprint.simp(a0[[1]], style=style(), char.dim=char.dim(a0, style=style()))
dprint.simp(a[[1]], style=style(), char.dim=char.dim(a, style=style()))
dprint.simp(a2[[1]], style=style(), char.dim=char.dim(a2, style=style()))
dprint.simp(a3b[[1]], style=style(), char.dim=char.dim(a3b, style=style()))
dprint.simp(a3c[[1]], style=style(), char.dim=char.dim(a3c, style=style()))

### CHAR.DIM ###
char.height("A", frmt=frmt())
char.dim(ac2, style=style())
char.dim(ac2, style=style(frmt.bdy=frmt(buf=c(2,3)))) # Mispecified buffer, to test recylce rule

(a <- tbl.struct(fmla=f1, data=table1, regx="1|2|.y"))
size.simp(a[[1]], char.dim(ac2, style=style()))

### TABLEBREAK ###
tablebreak(a1, 1, 4)
tablebreak(a2, 1, 4)

tl1 <- tbl.struct(fmla=f1, data=longtable1, label="level", group="group")
tablebreak(tl1, 1, 35)

###   DPRINT.TABLE   ###
dprint.table(fmla=f1, data=table1)
x11()
dprint.table(level~., table1)
x11()
dprint.table(fmla=f0, data=table1,row.hl=row.hl(1))
x11()
dprint.table(fmla=f1, data=table1,row.hl=row.hl(1))
dprint.table(fmla=f3c, data=table1, regx="1|2")

x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F)
x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=T, main="Table\ntitle")

## This section tests if justification is appropriately accounted for by observing text relation to the table border
# No group or Label
x11()
dprint.table(fmla=f0, data=table1,margins=.2, showmargins=F, main="Table Center",  row.hl=row.hl(c(1,4), "red"),style=style(frmt.tbl=frmt(bty="o"), frmt.col=frmt(bg="khaki")))

x11()
colnames(table1)
dprint.table(fmla=level~., data=table1,margins=.2, showmargins=F, main="Table Center",  row.hl=row.hl(c(1,4), "red"),style=style(frmt.tbl=frmt(bty="o"), frmt.col=frmt(bg="khaki")))

x11()
dprint.table(fmla=f0, data=table1,margins=.2, showmargins=F, main="Table Right", style=style(justify="right",frmt.tbl=frmt(bty="o")))

### HEADER TESTING ###
x11()
dprint.table(fmla=f1, data=table1,margins=c(1, 1, 1.25, 1), showmargins=T, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")))
hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5)), txt2="CONFIDENTIAL")
ftr("VNSNY:  The Center for Home Care\n               Policy & Research", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5)))

x11()
dprint.table(fmla=f0, data=table1,margins=c(1.25, 1), showmargins=T, main="Table Left",
             style=style(justify="left", frmt.tbl=frmt(bty="o")),
             f.hdr = expression(hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5)))))

h <- expression(hdr("Test Header", pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5))))
x11()
dprint.table(fmla=f0, data=table1,margins=c(1.25, 1), showmargins=T, main="Table Left",
             style=style(justify="left", frmt.tbl=frmt(bty="o")),
             f.hdr = h)


x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F, main="Table Center",
              style=style(frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(lty=2, lcol="light blue"), frmt.col=frmt(bg="khaki")))
x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F, main="Table Right", style=style(justify="right", frmt.tbl=frmt(bty="o")))
x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")))
x11()
# Underline column hiearchy
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F, main="Table Center", row.hl=row.hl(7),
                style=style(frmt.tbl=frmt(bty="o"), frmt.bdy=frmt(lty=2, lcol="light blue"), frmt.colh=frmt(bty="_")))
x11()
dprint.table(fmla=f1,row.hl=row.hl(table2$p.value <=.05), data=table1,margins=.2, showmargins=F, main="Table Right", style=style(justify="right", frmt.tbl=frmt(bty="o"), frmt.colh=frmt(bty="_")))
x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=F, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o"), frmt.colh=frmt(bty="_")))

x11()
dprint.table(fmla=group + level ~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2)+p.value, data=table1,margins=.2, showmargins=F, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")))



# Checking cnetring of column headings, since follows different logic than rest of the table when justify = right/left
x11()
dprint.table(fmla=f2, data=table1,margins=c(.2, .3, .4, .1), showmargins=T, main="Table Center", style=style(frmt.tbl=frmt(bty="o"), frmt.colh=frmt(bty="_"), frmt.col=frmt(bg="khaki")))
x11()
dprint.table(fmla=f2, data=table1,margins=.2, showmargins=F, main="Table Right", footnote="This table sucks",style=style(justify="right", frmt.tbl=frmt(bty="o")))
x11()
d <- dprint.table(fmla=f2, data=table1,margins=.2, showmargins=F, main="Table Left", footnote=c("* This table sucks\n and is too damn long", "* I hate making tables"),
              style=style(justify="left", frmt.tbl=frmt(bty="o")))


x11()
dprint.table(fmla=f1, data=table1,margins=.2, showmargins=T, style=style(justify="right"), main="Right")

### Conditional Group ###
x11()
dprint.table(fmla=group + level ~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2)+p.value|group2,
              data=table2,row.hl=row.hl(table2$p.value <=.05), margins=.2, showmargins=T, main="Table Left", style=style(justify="left",frmt.tbl=frmt(bty="o")))

x11()
dprint.table(fmla=group + level ~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2)+p.value,
              data=table2,row.hl=row.hl(c(7,12,13,14)),margins=.2, showmargins=T, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")),
             footnote="* This table sucks")


             x11()
dprint.table(fmla=group + level ~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2)+p.value|group2,
              data=table2,row.hl=row.hl(c(7,12,13,14)),margins=.2, showmargins=T, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")),
             footnote="* This table sucks")

x11()
dprint.table(fc3,
              data=table3,margins=.2, showmargins=T, main="Table Left", style=style(justify="left", frmt.tbl=frmt(bty="o")),
             footnote="* This table sucks")




CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans", text.all=T, bty="X"), frmt.tbl=frmt(bty="o"),
             frmt.col=frmt(bg="khaki", fontface="bold"), frmt.grp=frmt(bg="khaki", fontface="bold"))
x11()
dprint.table(fmla=f1, data=longtable1, margins=.2, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), main="Long Ass Table")
# Fit Width
x11()
dprint.table(fmla=f1, data=longtable1, margins=.2, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), fit.width=T, main="Fit Width")
# Fit Height
x11()
dprint.table(fmla=f1, data=longtable1, margins=.2, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05),
             fit.height=TRUE, main="Fit Height")
x11()
dprint.table(fmla=f1, data=longtable1, showmargins=T,row.hl=row.hl(longtable1$p.value<=.05),
             fit.height=TRUE, main="Fit Height")
# Fit - takes min(cex.width, cex.height)
x11()
dprint.table(fmla=f1, data=longtable1, margins=1.5, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), fit=T, main="Fit Width")


### multipage ###
x11()
dprint.table(fmla=f1, data=longtable1,showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), newpage=TRUE)
# fIT width and multipage
x11()
dprint.table(fmla=f1, data=longtable1, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), newpage=TRUE, fit.width=TRUE)


x11()
dprint.table(fmla=f1, data=longtable1, showmargins=T, style=CBs,row.hl=row.hl(longtable1$p.value<=.05), newpage=TRUE, fit.width=TRUE)

f <- expression(ftr("VNSNY:  The Center for Home Care\n                Policy & Research",
                    pagenum=eval.parent(pagenum, 1)))
h <- expression(hdr("Test Header for Report"))

pdf("longtable1.pdf", height=11, width=8.5)
  dprint.table(fmla=f1, data=longtable1, showmargins=T, dtype="portrait",
               style=CBs,row.hl=row.hl(longtable1$p.value<=.05), newpage=TRUE, fit.width=TRUE,
               f.hdr=h, f.ftr=f)
dev.off()

# Center table on page
pdf("longtable1-center.pdf", height=11, width=8.5)
  dprint.table(fmla=group+level~Mean1+Variance1, data=longtable1, showmargins=T, dtype="portrait", margins=1,
               style=CBs,row.hl=row.hl(longtable1$p.value<=.05), newpage=TRUE, center=TRUE)
dev.off()

 y ~ ., data = freeny
