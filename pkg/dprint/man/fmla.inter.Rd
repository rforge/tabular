\name{fmla.inter}
\alias{fmla.inter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula Interface Describing the Structure of Tabular Data
%%  ~~function to do ... ~~
}
\description{Parse the left and right hand side of the formula.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmla.inter(f, data = NULL, regx = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{formula
%%     ~~Describe \code{f} here~~
}
  \item{data}{data frame
%%     ~~Describe \code{data} here~~
}
  \item{regx}{regular expression to be removed from existing column names
%%     ~~Describe \code{regx} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Operator Definition:
"~" = separates row versus column definitions (LHS vs RHS of equation) Left Hand Side (LHS) – row attributes
"+" =  delimits up to two row labeling hierarchies (group + level) Right Hand Side (RHS) – column attributes
"+" =  delimits column placement
":"  =  spanning attribute of a columns
"."  =  all columns in data frame should be included
"-"  =  drops following column
"|"  =  list variables (delimited by "+") to condition on when print multiple simple tables

Embedded Functions to replace tedious preprocessing steps before presentation
Fc, Fb, Fci = Formatting, Rounding & Place holders concatenating one of more fields with other symbolic text
Rn = Renaming column names from cryptic variables names to presentable labels. Rn(column name, "new name")
%%  ~~ If necessary, more details than the description above ~~
}
\value{list of elements to be bassed to tbl.struct
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Carlin
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fmla.lhs}}
\code{\link{fmla.rhs}}
\code{\link{tbl.struct}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
fmla.inter(~., table1)            # print everything in data frame
fmla.inter(level~., table1)       # level and everything
fmla.inter(group+level~., table1) # row dimensions exist identfied by group & level,  and everything else is printed
fmla.inter(~Mean1 + Median1 + Variance1, table1) # No group or level + subset of fields on the right
fmla.inter(group+.~ Mean1 + Median1 + Variance1, table1)     #group, but no level, and subet of fields on right
fmla.inter(group+level~ Mean1+Median1, table1)
fmla.inter(group+level~ Mean1 + Median1 + Variance1, table1) # group and level, and subset of fields on right

### Spanning ###
fmla.inter(group+level~ Control:(Mean1 + Median1 + Variance1), table1) # group level, and apply Control as hierachy to fields on right
fmla.inter(group+level~ Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, table1) # group level, and apply control and treatments to hierchaies on right
fmla.inter(group+level~ Control:(Mean1 + Median1 + Variance1) + Treatment:(Mean2 + Median2 + Variance2) + p.value, table1) # group level, and apply control and treatments to hierchaies on right
# Illegal name for formula, and use regualr expression to take of text.  This will be useful to prevent erros when tie brekers
fmla.inter(group+level~ `This is a Control`:(Mean1 + Median1 + Variance1) + Treatment.y:(Mean2 + Median2 + Variance2), table1, regx="1|2|.y")
fmla.inter(group+level~.-p.value, table1) # all on rhs with exception of p.value

fmla.inter(~.-p.value, table1)   # Columns but p.value

### Condition logic ###
# Condition on group2
fmla.inter(group+level~ Mean1 + Median1 + Variance1|group2, table2)
# Condition on group2 and group3
fmla.inter(group+level~ Mean1 + Median1 + Variance1|group3+group2, table3)
### Note condition accross is not implemented in the presentation or structure of package, only here in formula itnerface
# Condition on group2 down, and group3 accross page (not yet implemented in presentation)
fmla.inter(group+level~ Mean1 + Median1 + Variance1|group2|group3, table3)
fmla.inter(group+level~ Mean1 + Median1 + Variance1|.|group3, table3) # Just across

fmla.inter(level~.|group2, table2)
fmla.inter(~.|group2, table2)
fmla.inter(group+.~.|group2, table2)
fmla.inter(group+.~.|.|group3, table3)
fmla.inter(group+level~.|group2, table2)
fmla.inter(group+level~x:(Mean1+Median1)+y:(Mean2+Median2)|group2, table2)

### Rename and other imbedded functions ##
fmla.inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla.inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), table1)
fmla.inter(group+level~Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)"), table1)
# Spanning + Embedded
fmla.inter(group~Treatment:Rn(round(Mean1, 2), "Mean Trt"), table1)
fmla.inter(group~Treatment:Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla.inter(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)"), table1)
fmla.inter(group+level~Control:(Mean1 + Median1 + Variance1), table1)
fmla.inter(group+level~Rn(round(Mean1, 2), "Mean Trt")+Variance1, table1)
fmla.inter(group+level~Rn(round(Mean1, 2), "Mean\nTrt")+Variance1, table1)
fmla.inter(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")|group2, table2)


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(f, data=NULL, regx=NA, ...) {
  level       <- NULL  # Character, name of column containing row labels
  group       <- NULL  # first level grouping
  byvars      <- NULL
  rhs.lpad    <- FALSE
  grp         <- NULL
  lvl         <- NULL
  lvl.v <- NULL
  byvars1.v <- NULL
  byvars2.v <- NULL
  fl = as.list(f)
  rl = list()
  l = length(fl)
  rhs <- if(l > 2) fl[[3]] else fl[[2]]
  lhs <- if(l > 2) fl[[2]] else NULL

  #  indicate if there is a group and level variable on the lhs
  ## rhs.lpad indicates if level variable is present - that must be moved to the rhs of the formula for model.frame to work
  #just group
  if (length(all.vars(lhs)) == 1 )
  {
    rhs.lpad = FALSE
    lvl = all.vars(lhs)[1]
  }
  #both group and level, need to move level to rhs
  if (length(all.vars(lhs)) > 1 )
  {
    rhs.lpad = TRUE
    grp = all.vars(lhs)[1]
    lvl = all.vars(lhs)[2]
  }

  rhs.obj      <- fmla.rhs(rhs, span=FALSE) # Removes spanning from formula
  rhs.obj.span <- fmla.rhs(rhs, span=TRUE)  # Keeps spanning in

  # Exception for when all variables are used
  rhs      = rhs.obj$rl
  rhs.span = rhs.obj.span$rl
  Rn.o <- as.character(unlist(rhs.obj$Rnl$Rn.o))
  Rn.n <- rhs.obj$Rnl$Rn.n

  if (!is.null(lhs))
  {
    lhs = fmla.lhs(lhs, grp, lvl, rhs.lpad)
    fmla      = as.call(list(as.symbol("~"), lhs, rhs))
    fmla.span = as.call(list(as.symbol("~"), lhs, rhs.span))
  }
  else
  {
    fmla      = as.call(list(as.symbol("~"), rhs))
    fmla.span = as.call(list(as.symbol("~"), rhs.span))
  }

  fmla.span     <- as.formula(fmla.span)
  fmla          <- as.formula(fmla)
  ### Column Hiearchy ###
  trms          <- terms(fmla.span, data=data, keep.order=T)    # returns terms obect
  t.trmlbls     <- attr(trms, "term.labels")                    # returns labels of terms on rhs
  # Exception to have group and no level, "." is place holder here for level
  if (!is.null(lvl))
    {if (lvl=="."){lvl<- NULL}
     else {lvl.v <- data[,lvl]} # save vector to add to data frame since model.frame can not handle custom lhs with two variables
     }
  if (!is.null(rhs.obj$byvars$byvars1)) {byvars1.v <- data[,rhs.obj$byvars$byvars1]}
  if (!is.null(rhs.obj$byvars$byvars2)) {byvars2.v <- data[,rhs.obj$byvars$byvars2]}
  #  Need to subtract byvars from terms
  #  Exception for formula of the type y~.|group2.
  if (sum(!is.null(rhs.obj$byvars$byvars1), !is.null(rhs.obj$byvars$byvars2), !is.null(lvl)))
  {
    byvartrms <- which(t.trmlbls \%in\% c(rhs.obj$byvars$byvars1, rhs.obj$byvars$byvars2, lvl))
    if (length(byvartrms)>0) {t.trmlbls <- t.trmlbls[-byvartrms]}
  }

  colnames.obj  <- colnames.struct(t.trmlbls, FALSE)
  if (length(Rn.o) > 0)
   {for (rn.i in 1:length(Rn.o)) {colnames.obj$cname[colnames.obj$cname == Rn.o[rn.i]] <- Rn.n[rn.i]}}

  # Line Break in Rename Text
  colnames.obj <- colnames.linebreak(colnames.obj)
  colnames.obj <- colnames.row(colnames.obj)       #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
  if (!is.na(regx)) {colnames.obj$cname <- kill.multiregx(colnames.obj$cname, regx=regx)}
  # Use model.frame to apply embedded functions
  trms.cn <- attr(terms(fmla, data=data), "term.labels")
  data=model.frame(fmla, data)
  # Drops variables from data frame when "-" was used
  cn.dx <- which(colnames(data) \%in\% c(grp, trms.cn))
  data <-data[,cn.dx]
  if (!is.data.frame(data))
    {data <- as.data.frame(data)}
  if(!is.null(lvl)) {data[,lvl] <- lvl.v}
  if(!is.null(byvars1.v)) {data[,rhs.obj$byvars$byvars1] <- byvars1.v}
  if(!is.null(byvars2.v)) {data[,rhs.obj$byvars$byvars2] <- byvars2.v}
  return(list(tbl=data, group=grp, label=lvl, byvars1=rhs.obj$byvars$byvars1, byvars2=rhs.obj$byvars$byvars2, fmla=fmla, colnames.obj=colnames.obj))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
