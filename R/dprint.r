library(grid)

# ============== #
#       RDESC    #
# ============== #
# PURPOSE:  Creates a sample data frame to represent a table of descriptive statistics for debugging
rdesc <- function(n.grp = 8, n.lvls = 5, n.grp2=NULL, rnd=FALSE)
  {
    g  <- rep(LETTERS[1:n.grp], rep(n.lvls, n.grp))
    if (!is.null(n.grp2)) {g2 <- rep(c(1:n.grp2), rep(ceil(length(g)/n.grp2), n.grp2))}
    f  <- rep(letters[(26-n.lvls+1):26], n.grp)
    f <- paste(f,f,f,f,f,f,f,f,f, sep="")
    mn1   <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    mdn1  <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    var1  <- rchisq((n.lvls*n.grp), 1.1)
    mn2   <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    mdn2  <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    var2  <- rchisq((n.lvls*n.grp), 1.3)
    pval  <- runif((n.lvls*n.grp), max = .25)
    table <- data.frame(group = g, level = f, Mean1 = mn1, Median1= mdn1, Variance1 = var1, Mean2 = mn2, Median2 = mdn2, Variance2 = var2, "p-value" = pval)
    if (!is.null(n.grp2))  {table$group2 <- g2}
    if(rnd) {    table[ , 3:9] <- prettyNum(round(table[ , 3:9], 2), drop0trailing=FALSE)}
    else {table[ , 3:9] <- table[ , 3:9]}
    return(table)
  }

rbin <- function(n.grp = 8, n.lvls = 5, n.grp2=NULL)
  {
    g  <- rep(LETTERS[1:n.grp], rep(n.lvls, n.grp))
    if (!is.null(n.grp2)) {g2 <- rep(c(1:n.grp2), rep(ceil(length(g)/n.grp2), n.grp2))}
    f  <- rep(letters[(26-n.lvls+1):26], n.grp)
    f <- paste(f,f,f,f,f,f,f,f,f, sep="")
    r1 <- rpois((n.lvls*n.grp), 20)
    n1 <- rpois((n.lvls*n.grp), 100)
    n1 <- ifelse(n1<r1, r1, n1)
    p1 <- r1/n1
    r2 <- rpois((n.lvls*n.grp), 30)
    n2 <- rpois((n.lvls*n.grp), 100)
    n2 <- ifelse(n2<r2, r2, n2)
    p2 <- r2/n2
    pval  <- runif((n.lvls*n.grp), max = .25)
    table <- data.frame(group = g, level = f, r1=r1, n1=n1, p1=p1, r2=r2,n2=n2, p2=p2, "p-value" = pval)
    if (!is.null(n.grp2))  {table$group2 <- g2}
    return(table)
  }
  
# ============================== #
#  Format Functions              #
# ============================== #
Fc <- function(mn, std, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(std, digits), digits=digits, nsmall=nsmall), ")", sep="")
  if (NAmiss) { f[is.na(mn)] <- ""}
  f
}

Fci <- function(cil, ciu, mn=NA, digits=2, nsmall=2, NAmiss=TRUE)
{
  if (is.na(mn)) {f <- paste("(", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  else{f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  if (NAmiss) { f[is.na(cil)] <- ""}
  f
}

# I made a change here, not sure if working 10/22/11
Fb <- function (prop=NULL, r=NULL, n=NULL, digits.p=4, nsmall.p=2, digits.n=6)
  {

    if (!is.null(n) & !is.null(r))
      {f <- paste(format(round(100*r/n, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else if (!is.null(n))
      {f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else
      {{f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% ", sep="")}}
    f
  }
  

# ==================================== #
#             FMLA.INTER               #
# ==================================== #
# PURPOSE:  Creates a user friendly interface to TBL.STRUCT
##  fmla.inter manages the right hand side and left hand side of the equation with fmla.rhs, fmla.lhs
# Rules for forumula:
## LHS of formula contains the level name the first position after '~', and each group name is to the left of that separated by '+'
## RHS:
# ~. = entire data frame is printed with no label or group characters defined
# +  = deliminates all column to be printed
# -  = all but variables after minus sign
# :  = text to span columnes

#----------------#
###  FMLA.LHS  ###
#----------------#
#  parse left hand side of formula
fmla.lhs <- function(f, grp=NULL, lvl=NULL, rhs.lpad=FALSE) {

  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0
  colon.op  = FALSE
  Rn.op     = FALSE
  lvl.found = FALSE

  if (l == 1) return(f)
  for (tm in fl) {
    i=i+1
    if (i==1) {
      rl[[i]] = tm
      if (tm == ":")  colon.op = TRUE
      if (tm == "Rn") Rn.op = TRUE
      if (tm == "|")  rl[[i]] = as.symbol("+")
    }
    if (i > 1) {
      if (typeof(tm) == "language")
           {rl[[i]] = fmla.lhs(tm)
           }
      else #group or level
           if (tm == lvl) lvl.found = TRUE
             rl[[i]] = tm
    }
  }
  if (colon.op == TRUE)  return(rl[[3]]) #third element is the variables that are spanned
  if (Rn.op == TRUE)     return(rl[[2]]) #second element is variables being renamed
  if (rhs.lpad == TRUE && lvl.found == TRUE) return(rl[[2]]) #second element is level
  return(as.call(rl))
}

#----------------#
###  FMLA.RHS  ###
#----------------#
#  PURPOSE:  parse right hand side of formula
fmla.rhs <- function(f,
                  span, # There are two types of calls, when TRUE returns the spanning text, does not when FALSE
                  Rnl=list(Rn.o=list(), Rn.n=NULL, rn.i=0),
                  byvars=list(byvars1=NULL, byvars2=NULL, byvars.i=0)
                  )
 {
  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0;
  colon.op  = FALSE  # uses colon operator
  Rn.op     = FALSE  # uses Rn function (rename)
  cond.op   = FALSE  # uses | operator
  period.op = FALSE  # uses . operator to indicate all
  I.op      = FALSE  # Some type of other function should be kept as expression
  for (tm in fl)
    {
      i=i+1
      if (i==1)
        {
          rl[[i]] = tm
          if (tm == ".")  {period.op = TRUE;}
          if (tm == ":")  {colon.op = TRUE}
          if (tm == "Rn")
            {Rn.op = TRUE;
             # Store old name(expression) with new name and update rename index
             Rnl$rn.i<-Rnl$rn.i+1
             Rnl$Rn.o[[Rnl$rn.i]] <- fl[[2]]
             Rnl$Rn.n <- c(Rnl$Rn.n, fl[[3]])
             }
          if (tm == "|")
          {
            cond.op = TRUE
            rl[[i]] = as.symbol("+"); # Replace symbol so model.frame will keep column in data.frame
            byvars$byvars.i=byvars$byvars.i+1;
            if (byvars$byvars.i==1) {byvars$byvars1 <-  all.vars(fl[[3]])}
            if (byvars$byvars.i==2)
            {
              byvars$byvars2 <- byvars$byvars1
              byvars$byvars1 <- all.vars(fl[[3]])
              if (byvars$byvars1==".") {byvars$byvars1<-NULL}
            }
          }
          if (sum(colon.op, Rn.op, cond.op, period.op)==0) {I.op<-TRUE}
        }
      if (i > 1)
        { # If elements of formula are still functinos call again
          if (typeof(tm) == "language")
           {
             rhs.obj <-  fmla.rhs(tm, span=span, Rnl=Rnl, byvars=byvars)
             rl <- c(rl, rhs.obj$rl)
             Rnl = rhs.obj$Rnl
             byvars = rhs.obj$byvars
           }
          else rl[[i]] = tm
        }
   }
  # When span = FALSE, remove spanning text from formula
  if (!span) {if (colon.op) {return(list(rl=rl[[3]], Rnl=Rnl, byvars=byvars))}}
  # Rename, second element is variables being renamed
  if (Rn.op)    {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  if (cond.op)  {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  # Any other function/operator not defined here, treat as is
  if (I.op)     {return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))}

  # Last Call of Recursion
  if (period.op)
  {
    return(list(rl=as.symbol(rl[[1]]), Rnl=Rnl, byvars=byvars))
  }
  return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))
}

#------------------#
###  FMLA.INTER  ###
#------------------#
# PURPOSE:  parse lhs/rhs: expect formula of form:  lhs ~ rhs
# if 2 vars on lhs, need to move lvl to the rhs. model.frame will generate an error if 2 factors on lhs
fmla.inter <- function(f, data=NULL, regx=NA, ...) {
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
    byvartrms <- which(t.trmlbls %in% c(rhs.obj$byvars$byvars1, rhs.obj$byvars$byvars2, lvl))
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
  cn.dx <- which(colnames(data) %in% c(grp, trms.cn))
  data <-data[,cn.dx]
  if (!is.data.frame(data))
    {data <- as.data.frame(data)}
  if(!is.null(lvl)) {data[,lvl] <- lvl.v}
  if(!is.null(byvars1.v)) {data[,rhs.obj$byvars$byvars1] <- byvars1.v}
  if(!is.null(byvars2.v)) {data[,rhs.obj$byvars$byvars2] <- byvars2.v}
  return(list(tbl=data, group=grp, label=lvl, byvars1=rhs.obj$byvars$byvars1, byvars2=rhs.obj$byvars$byvars2, fmla=fmla, colnames.obj=colnames.obj))
}


# ==================================== #
#             LIST.TO.DF               #
# ==================================== #
# PURPOSE: Creates a data frame from a list of vectors of varying lengths.  Elements are NA where lengths of vectors are smaller than largest
list.to.df <- function(lst # this is a list returned from string split
                                  )
{
  lst.df <- NULL
  if (sum(unlist(lapply(lst, FUN=function(x) {length(x)}))) > 0)
  {
    mx <- max(unlist(lapply(lst, FUN=function(x) {length(x)})))
    for (i in 1:length(lst))
    {
     lst[[i]] <- c(lst[[i]], rep(NA, mx-length(lst[[i]])))
    }
    lst.df <- t(as.data.frame(lst, stringsAsFactors =F))
    colnames(lst.df) <- paste("C", 1:ncol(lst.df), sep="")
    rownames(lst.df) <- paste("R", 1:nrow(lst.df), sep="")
  }
  lst.df
}

# ==================================== #
#       CONSECT.STRUCT                 #
# ==================================== #
# PURPOSE:   Given a vector returns several objects describing how(if) consecutive elements appear.  This
#            is a helper function for TBL.STRUCT and is important in defining the column hierarchy.
consect.struct <- function(vct  # vector
                          )
{
  temp.vct <- vct
  lvct <- length(vct) #length of vector
  # Handle NAs
  temp.vct[is.na(vct)] <- paste("NA.", 10000+(1:length(which(is.na(vct)))), sep="") # NA's should be treated as distinct
  next.vct  <- c(temp.vct[2:lvct], NA) # the next position
  # Indicate when the next consective value begins
  new.vct       <- temp.vct != next.vct
  new.vct       <- c(TRUE, new.vct[-lvct])
  new.vct.dx    <- which(new.vct)
  new.vct.gdx   <- cumsum(new.vct)
  consec.info   <- data.frame(vct, temp.vct, next.vct, new.vct, new.vct.gdx, stringsAsFactors =F)
  if (lvct>1) # Exception handler for when just vector is of length 1
  {
    min.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {min(x)})
    max.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {max(x)})
  }
  else {min.dx=1; max.dx=1}
  consec.begend <- data.frame(uniq.val=vct[new.vct], mn=min.dx, mx=max.dx, stringsAsFactors =F)
  return(list(consec.info=consec.info, consec.begend=consec.begend[!is.na(consec.begend$uniq.val), ]))
}


# ==================================== #
#       COLNAMES.LINEBREAK             #
# ==================================== #
# PURPOSE: Inserts a line break in column names when the escape character "\n" is found.
# NOTE:    Recursive function
colnames.linebreak <- function(colnames.obj  # Column Names Object creatd by COLNAMES.STRUCT
                              )
{
  grep.linebreak <- grep("\n",colnames.obj$cname)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak  <- strsplit(colnames.obj$cname[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak         <- length(strsplit.linebreak[[1]]) # store how many line breaks
       d.temp         <- colnames.obj[rep(grep.linebreak[1], n.linebreak), ] # Expand for each linebreak in this particular name
       d.temp$cname   <- strsplit.linebreak[[1]] # insert section of string that was delimitted by the string break character
       d.temp$col.row <- n.linebreak:1
       if (grep.linebreak[1] == 1)
        {  # Insert new expanded rows into orignal object (if row occurs at begining of data frame)
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind( d.temp, d.after)
        }
       else if (grep.linebreak[1] == nrow(colnames.obj))
        {  # Insert new expanded rows into orignal object (if row occurs at end of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          colnames.obj <- rbind(d.before,  d.temp)
        }
       else
        {  # Insert new expanded rows into orignal object (if row occurs in middle of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind(d.before,  d.temp, d.after)
        }
        grep.linebreak <- grep("\n",colnames.obj$cname)
        # Recursive Calls
        if (length(grep.linebreak) > 0) {colnames.obj <- colnames.linebreak(colnames.obj)}
    }
  colnames.obj
}

# ==================================== #
#             COLNAMES.ROW             #
# ==================================== #
# PURPOSE:  Adjust index (reference # of rows above table for colnames) to account for line breaks
colnames.row <- function(colnames.obj)
  {
     max.row <- tapply(colnames.obj$col.row, list(colnames.obj$col.logical.row), function(x) max(x, na.rm=T))
     d.max.row <- data.frame(max.row)
     d.max.row$col.logical.row <- rownames(d.max.row)
     d.max.row$lag.max.row.adj <- c(0, d.max.row$max.row[-nrow(d.max.row)]-1)
     colnames.obj <- merge(colnames.obj, d.max.row, by ="col.logical.row")
     colnames.obj$row <- colnames.obj$col.logical.row + colnames.obj$col.row -1 + colnames.obj$lag.max.row.adj
     colnames.obj$row <- max(colnames.obj$row) - (colnames.obj$row-1)
     colnames.obj
  }


# ==================================== #
#           COLNAMES.STRUCT            #
# ==================================== #
# PURPOSE:  Creats a structure for printing column names and their hierachies
colnames.struct <- function(col.names, linebreak=TRUE)
{
  # look for escape character indicating that a grouping scheme exists for column names
  col.grp.dx      <- grep(":", col.names)
  col.grp.split   <- strsplit(col.names, ":")
  orig.names      <- unlist(lapply(col.grp.split, FUN=function(x) {x[length(x)]}))
  lst.names       <- lapply(col.grp.split, FUN=function(x) {x[-length(x)]}) # group names (if any) after removing origninal column names
  lnames          <- length(orig.names)
  # Logical row keeps column hiercahies together, col.row indicates whether line breaks have been performed
  column.heading  <- data.frame(cname = orig.names, col.logical.row = rep(1, lnames), col.row=1, span.beg=1:lnames, span.end=1:lnames, stringsAsFactors =F)
  r.i             <- nrow(column.heading)
  cname.hierc.df  <- list.to.df(lst.names)

  if(!is.null(cname.hierc.df))
    {
      for (hier.i in ncol(cname.hierc.df):1)
        {
          d.i <- consect.struct(cname.hierc.df[, hier.i])
          n.d <- nrow(d.i$consec.begend)
          column.heading[r.i+ (1:n.d), "col.logical.row"] <- ncol(cname.hierc.df)-hier.i+2
          column.heading[r.i+ (1:n.d), "col.row"] <-1
          column.heading[r.i+(1:n.d), c("cname", "span.beg", "span.end")] <- d.i$consec.begend
          r.i <- nrow(column.heading)
        }
    }
  column.heading$cname  <- kill.multiregx(column.heading$cname, "`") #  Remove backhashes
  if (linebreak) # Can be called in two different places with different assumptions whether the line break has been added yet
    {
      column.heading        <- colnames.linebreak(column.heading)        #  Account for line breaks in column names
      column.heading        <- colnames.row(column.heading)              #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
    }
  column.heading
}

# ==================================== #
#          VECTOR.LINEBREAK            #
# ==================================== #
# PURPOSE:  To break vectors apart around an escape character "\n" that indicates a line break.  This is designed for handling line breaks for
#           the table title (main) and the footnotes.
vector.linebreak <- function(vctr)
{
  grep.linebreak <- grep("\n",vctr)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak   <- strsplit(vctr[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak          <- length(strsplit.linebreak[[1]]) # store how many line breaks
       v.temp               <- strsplit.linebreak[[1]]
       if (length(vctr)==1) {vctr <- v.temp}
       else
         {
           if (grep.linebreak[1] == 1)
            {  # Insert new expanded rows into orignal object (if occurs at begining of data frame)
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.temp, v.after)
            }
           else if (grep.linebreak[1] == length(vctr))
            {  # Insert new expanded rows into orignal object (if occurs at end of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              vctr      <- c(v.before, v.temp)
            }
           else
            {  # Insert new expanded rows into orignal object (if occurs in middle of data frame)
              v.before  <- vctr[1:(grep.linebreak[1]-1)]
              v.after   <- vctr[(grep.linebreak[1]+1):length(vctr)] # After
              vctr <- c(v.before, v.temp, v.after)
            }
            grep.linebreak <- grep("\n",vctr)
            # Recursive Calls
            if (length(grep.linebreak) > 0) {vctr <- vector.linebreak(vctr)}
         }
    }
  vctr
}

# ==================================== #
#              vector.struct             #
# ==================================== #
vector.struct <- function(vctr=NA)
{
  vctr.nrw=NA
  if (!is.na(vctr[1]))
    {
      vctr      <- vector.linebreak(vctr)
      vctr.nrw  <- length(vctr)
    }
  return(list(vctr=vctr,          # Newly formatted title
              vctr.nrw=vctr.nrw   # Number of rows title will take
              ))
}

# ==================================== #
#               INSERT.GRP             #
# ==================================== #
# PURPOSE:  Inserts a value into record in tbl.struct objects to control for when empty rows should be inserted into the data structure.  NA's will be suppressed
#           during presentation of the table.
insert.grp1 <- function(obj,      # vector or data frame
                      dx,         # Index where NA is to be inserted
                      lblescp,    # Label Escape, boolean
                      group=F,  # indicates that this object is related to the grouping vector and the insetions must go after
                      val=NA,
                      dx.up =FALSE # If true adjust an index
)
{
  # This section handles data frames
  if (is.data.frame(obj))
    {
      for (i in 1:length(dx))
        {
          if (!lblescp[i])
            { if(i==1)  {obj <- rbind(rep(val, ncol(obj)), obj)}
              else
                { # Exception to handle object changing class when subsetting
                  data1 <- obj[1:(dx[i]-1), ]
                  data2 <- obj[(dx[i]):nrow(obj), ]
                  if (!is.data.frame(data1)) {data1 <- as.data.frame(data1); colnames(data1) <- "a1";}
                  if (!is.data.frame(data2)) {data2 <- as.data.frame(data2); colnames(data2) <- "a1";}
                   obj <- rbind(data1, rep(val, ncol(obj)), data2)
                }
              dx <- dx + 1
            }
        }
    }
  else if(dx.up)
    { obj2 <- obj
      for (dx.i in 1:length(dx))
        {
          if (!lblescp[i])
            {          obj2[obj>=dx[dx.i]] <- obj2[obj>=dx[dx.i]] +1}
        }
      obj<-obj2
    }
  # This section handles vectors
  else
    { # This section handles vectors with the exception of groups
      if (!group)
        {
          for (i in 1:length(dx))
            {
              if (!lblescp[i])
               {
                  if(i==1) {obj <- c(val, obj)}
                  else {obj <- c(obj[1:(dx[i]-1)], val, obj[(dx[i]):length(obj)])}
                  dx <- dx + 1
                }
            }
        }
      else # This section handles the group vector, index is slightly altered to put NA after the group name
        {
          for (i in 1:length(dx))
            {
              if (!lblescp[i])
               {
                  if(i==length(dx)) {obj <- c(obj, val)}
                  else {obj <- c(obj[1:(dx[i+1]-1)], val, obj[(dx[i+1]):length(obj)])}
                  dx <- dx + 1
               }
            }
        }
    }
  return(obj)
}


## ==================================== #
##               insert.grp              #
## ==================================== #
# PURPOSE:  Inserts NA positions for tbl, label, and group structure from tbl.obj
insert.grp <- function(tbl.obj)
{
  if (!is.null(tbl.obj$group))
   {
      tbl.obj$bdy         <- insert.grp1(tbl.obj$bdy, tbl.obj$newgrp.dx, tbl.obj$lblescp)
      if (!is.null(tbl.obj$label))
        {tbl.obj$label  <- insert.grp1(tbl.obj$label, tbl.obj$newgrp.dx, tbl.obj$lblescp)}
      tbl.obj$group       <- insert.grp1(tbl.obj$group, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T)
      tbl.obj$newgroup    <- insert.grp1(tbl.obj$newgrp, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T, val=FALSE)
      tbl.obj$lastofgroup <- insert.grp1(tbl.obj$lastofgroup, tbl.obj$newgrp.dx, tbl.obj$lblescp, val=FALSE)
      tbl.obj$nrw         <- tbl.obj$nrw + length(tbl.obj$newgrp.dx) - sum(tbl.obj$lblescp)
      if(!is.null(tbl.obj$row.hl$dx)) # Row Highlight
        {tbl.obj$row.hl$dx <- insert.grp1(tbl.obj$row.hl$dx, tbl.obj$newgrp.dx, tbl.obj$lblescp, FALSE,val=1, dx.up=TRUE)}
      # This must go last
      {tbl.obj$newgrp.dx <- which(tbl.obj$newgroup)}
   }
 return(tbl.obj)
}


# ==================================== #
#         TBL.STRUCT.SIMP              #
# ==================================== #
# PURPOSE:  decomposes the data.frame into the structure required by DPRINT
tbl.struct.simp <- function(data, label = NULL, group = NULL, main=NA, footnote=NA, colnames.obj=NULL)
  {
    newgrp       <- NULL # boolean, when new group starts
    newgrp.dx    <- NULL # index, when new group starts
    newgrp.gdx   <- NULL # Unique identifier for group
    lastofgroup  <- NULL # boolean, when last level of group
    ngrp         <- NULL # number of groups
    lblescp      <- rep(FALSE, nrow(data)) # boolean, when label should be escaped
    lblescp.dx   <- NULL # index, when label should be escaped
    cond.txt     <- NULL # Text from conditional group
    ### Row Label Hierarchy ###
    # If no label or group vector defined, tbl is original data
    if (is.null(label) & is.null(group))
      {bdy <- data}
    else
      { # If label or group vector defined in data frame, than remove
        bdy <- data[ , -which(colnames(data) %in% c(label, group))]
        if (!is.data.frame(bdy))
        {
         bdy <- as.data.frame(bdy)
         colnames(bdy) <- colnames(data)[-which(colnames(data) %in% c(label, group))]
         }
        if(!is.null(label))
          { label <- paste("  ",as.character(data[ , label]), sep="")}
        if(!is.null(group))
          { # Place an empty character on group so that text is not on top of the border, this is the best place since will be inbedded in on nhcar calcs
            group <- paste("  ", as.character(data[ ,group]), sep="")
            # Indicate when the group changes
            l.g          <- length(group)
            prev.grp     <- group[1:(l.g-1)]
            newgrp       <- c(TRUE, group[2:l.g] != prev.grp)
            lastofgroup <- c(newgrp[-1], TRUE)
            newgrp.dx    <- which(newgrp)
            ngrp         <- length(newgrp.dx)
            newgrp.gdx   <- cumsum(newgrp)
            # Handle label excape character
            group[!newgrp] <- NA
            if (!is.null(label))
              { lblescp    <- label == "  <<"         # boolean, when label escape character is present
                lblescp.dx <- which(lblescp)  # index, when escape character is present
                if (length(lblescp.dx) > 0)
                  {
                    label[lblescp.dx] <-NA         # Assign NA so it does not print a label for this one
                    dummy.sort <- 1:nrow(bdy)
                    dummy.sort[lblescp.dx] <- -1
                  }
              }
          }
      }


    ### Column Name Structure ###
    if (is.null(colnames.obj)) {colnames.obj <- colnames.struct(colnames(bdy))}
    nrw.colhead  <- max(colnames.obj$row)
    nrw <- nrow(bdy); ncl <- ncol(bdy);

    ### Main Structure ###
    main.obj <- vector.struct(main)
    ### Footnote Structure ###
    footnote.obj <- vector.struct(footnote)

    return(list(main=main.obj$vctr,           # Title
                main.nrw=main.obj$vctr.nrw,   # Number of rows title will take
                cond.txt=cond.txt,
                nrw = nrw, ncl = ncl,         # Number of rows & Columns in table
                colhead=colnames.obj,         # Structure to drive printing of column heading, created by COLNAMES.STRUCT
                nrw.colhead=nrw.colhead,      # Number of rows for column heading
                ngrp=ngrp, group = group, label = label, newgrp = newgrp, newgrp.dx=newgrp.dx,newgrp.gdx=newgrp.gdx,
                lastofgroup=lastofgroup,      # Indiates the last level of group
                lblescp=lblescp, lblescp.dx=lblescp.dx,
                bdy = bdy,
                footnote=footnote.obj$vctr,           # Footnote
                footnote.nrw=footnote.obj$vctr.nrw   # Number of rows footnote will take
              ))
  }


# ==================================== #
#             TBL.STRUCT               #
# ==================================== #
# PURPOSE:  Dispatch simple table structure (TBL.STRUCT.SIMP) controlling for conditional filters
# NOTE:     There are two ways to define a table structure, through the formula interface or by two parameters 'label' & 'group'
tbl.struct <- function(fmla=NULL,    # Formula interface to define table structure
                       data,         # Input Data.frame
                       label = NULL, # label & group are characters identifying columns that define the simple table structure
                       group = NULL,
                       regx=NA,      # Regular Expression to take off of colnames, designed to break unwanted tiebreakers for legal data.frame columnnames
                       main=NA,      # Table Title, Vector of strings where each element is a new line
                       footnote=NA,  # Footnote, Vector of strings where each element is a new line
                       row.hl=list(dx=NULL, col=NULL) #  Conditional Formatting to highlight rows
                       )
{
  tbl.obj <- vector("list", 1)
  ### Parameter Declaration ###
  if(is.null(fmla))
    {
      tbl.obj[[1]] <- tbl.struct.simp(data=data, label = label, group = group, main=main, footnote=footnote)
      # Conditional Formatting
      tbl.obj[[1]]$row.hl <- row.hl  # Row Highlight
  }
  ### Formula Interface ###
  else
    {
      fmla.obj   <- fmla.inter(fmla, data=data, regx=regx)
      # If no conidionalt variables than simple table structure
      if (is.null(fmla.obj$byvars1))
        {
          tbl.obj[[1]]  <- tbl.struct.simp(data=fmla.obj$tbl, label = fmla.obj$label, group = fmla.obj$group, main=main, footnote=footnote, colnames.obj=fmla.obj$colnames.obj)
          # Conditional Formatting
          tbl.obj[[1]]$row.hl <- row.hl  # Row Highlight
        }
      ### Condional Variables Used ###
      else # create a list of simple table structures by all combinations of values of conditional variables
        {
          conditional.obj     <- conditional.struct(fmla.obj$tbl, byvars=fmla.obj$byvars1)
           l.uniq.concat.cond  <- length(conditional.obj$uniq.concat.cond)
           tbl.obj             <- vector("list", l.uniq.concat.cond)
           data <- conditional.obj$data # Removes conditional variables

           for (uniq.concat.cond.i in 1:l.uniq.concat.cond)
            {
               cur.fltr.dx <- which(conditional.obj$concat.cond == conditional.obj$uniq.concat.cond[uniq.concat.cond.i])
               data.i <- data[cur.fltr.dx, ]
               if (!is.data.frame(data.i)) {data.i <- as.data.frame(data.i)} # Class change on subsetting nx1 data frame
               tbl.obj[[uniq.concat.cond.i]] <- tbl.struct.simp(data=data.i, label = fmla.obj$label, group = fmla.obj$group, main=main, footnote=footnote, colnames.obj=fmla.obj$colnames.obj)
               tbl.obj[[uniq.concat.cond.i]]$cond.txt <- conditional.obj$uniq.concat.cond[uniq.concat.cond.i]
               ### Conditional Formatting ###
               # Row highlight
               if (!is.null(row.hl$dx))
               {
                  tbl.obj[[uniq.concat.cond.i]]$row.hl <-list(dx=NULL, col=NULL)
                  row.hl.dx <- which(row.hl$dx <= max(cur.fltr.dx))
                  tbl.obj[[uniq.concat.cond.i]]$row.hl$dx  <- row.hl$dx[row.hl.dx]
                  tbl.obj[[uniq.concat.cond.i]]$row.hl$col <- row.hl$col
                  row.hl$dx <- row.hl$dx[-row.hl.dx]-nrow(data.i)
               }
            }
        }
    }
  tbl.obj
}

# ==================================== #
#       CONDITIONAL.STRUCT             #
# ==================================== #
# PURPOSE:  For variables defined as conditional variables in the formula, creates a structure to drive creating a list of simple table structures
conditional.struct <- function(data, byvars)
  {
    cnms <- colnames(data)[!colnames(data) %in% byvars]
    if (length(byvars)==1)
      { concat.cond <- paste(rep(byvars, ncol(data)), data[,byvars], sep="=")}
    else
      {
        l <- vector("list", length(byvars))
        for (byvars.i in 1:length(byvars))
          {
            l[[byvars.i]] <- paste(rep(byvars[byvars.i], ncol(data)), data[, byvars[byvars.i]], sep="=")
          }

          concat.cond <- apply(data.frame(l), 1, paste, collapse=", ")
      }
    uniq.concat.cond <- unique(concat.cond)
    data <- data[, -which(colnames(data) %in% byvars)]
    if (!is.data.frame(data))
      {data <- as.data.frame(data); colnames(data) <- cnms; }
   return(list(concat.cond=concat.cond, uniq.concat.cond=uniq.concat.cond, data=data))
  }




# ==================================== #
#               KILL.*                 #
# ==================================== #
#  PURPOSE:  These are utilitiy functions simplifying some regular expression logic


### KILL.MULTIREGX ###
kill.multiregx <- function(string, # string
                           regx # Perl Regular expression
                           )
{

  string <- sub(regx, '', string, perl = TRUE)
  g.spc <- grep(regx, string)
  if(length(g.spc) > 0)   { string <- kill.multiregx(string, regx) }
  return(string)
}


###################################################################### PRINTING ##########################################################################


# ==================================== #
#               FRMT                   #
# ==================================== #
# PURPOSE:  Returns a structure to control the gpar settings of GRID.* functions, as well as other formatting controls
# NOTE:     The parameters are set to the default settings
frmt <- function(fontfamily = "",
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

# ==================================== #
#               STYLE                  #
# ==================================== #
# PURPOSE:  This is how the user will change formatting of the printed table from a long list of defaults.
style <- function(frmt.bdy=NULL,
                  frmt.col=NULL,
                  frmt.colh=NULL, # Only used for borders around column spanning names( column hiearchy), "o" boxes entrie column, "_" puts line under hiarchy
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


# ==================================== #
#               DBORDER                #
# ==================================== #
# PURPOSE:  This allows the user to draw different types of borders around sections of table
# BTY options:  "o" = border arround entire plot
#               "=" = border top and bottom only
#               "_" = bottom only
#               "-" = top only

dborder <- function(cord1,  # Vector (x,y) indicating position of top left point of rectangle
                  cord2,  # Vector (x,y) indicating position of top left point of rectangle
                  frmt    #
                 )
{
  ### Prints a box - suppresse lines based on symbol ###
  if (frmt$bty %in% c("=", "o", "-"))
  {# TOP Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord1[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty %in% c("=","o", "_"))
  { # BOTTOM Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord2[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }

  if (frmt$bty %in% c("o"))
  {# LEFT Line
    grid.lines( x = unit(cord1[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty %in% c("o"))
  {# RIGHT Line
    grid.lines( x = unit(cord2[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
 }

pagelayout <- function(dtype="rgraphics", # Device type, currently available "rgraphics", "portrait", "landscape"
                       margins=NULL,   # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
                       pg.dim=NULL     # If custom page dimensions define a vector of c(width, height) units
                       )
{
  if (dtype=="rgraphics")
   {
      page.height <- 7; page.width  <- 7
      if (is.null(margins)) {margins = rep(.2,4)}
   }
  if (dtype == "portrait")
    {
     page.height = 11;  page.width  = 8.5;
     if (is.null(margins)) {margins = c(1, 1, 1.25, 1)}
    }
  if (dtype == "landscape")
    {
     page.height = 8.5; page.width  = 11;
     if (is.null(margins)) {margins = c(1, 0.5, 1, 0.5)}
    }

  # Custom page dimensions over ride dtype
  if (!is.null(pg.dim)) {pg.dim <- rep(pg.dim, 2)[1:2]; page.width<-pg.dim[1]; page.height<-pg.dim[2];}
  #  Handles different ways of defining margins common that left=right, and top =bottom, so c(1, .5) is shorter
  if (length(margins) < 4) {margins <- rep(margins, floor(4/length(margins)))[1:4]}
  # Top left, and bottom right cordinates
  cord.tl <- c(margins[2], page.height-margins[3]) # Top Left
  cord.br <- c(page.width-margins[4], margins[1]) # Bottom right

  return(list(margins=margins, page.height=page.height, page.width=page.width,cord.tl=cord.tl, cord.br=cord.br))
}

row.hl <- function(dx,
                   col="yellow")
  {
   if(is.logical(dx)) {dx<-which(dx)}
   return(list(dx=dx, col=col))
  }

# ==================================== #
#              CHAR.WIDTH              #
# ==================================== #
# PURPOSE:  Returns the max width in units, given on a gpar setting
char.width1 <- function(vctr, frmt, cx=1)
{

   if (is.factor(vctr)) {vctr <- as.character(vctr)}

   n.char   <- nchar(vctr)
   cwidth.n <- max(n.char, na.rm=T)
   max.dx   <- which.max(cwidth.n)

   cwidth.u <- max(unlist(lapply(vctr,FUN=function(x) {convertWidth(grobWidth(textGrob(x, gp=gpar(fontfamily = frmt$fontfamily,
                                                                     fontsize = frmt$fontsize,
                                                                     fontface = frmt$fontface,
                                                                     cex = cx
                                                                      ))), "inches", valueOnly =TRUE)})), na.rm=TRUE)

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}
# PURPOSE:  Returns the max width in units, handles data types
char.width <- function(obj, frmt, cx=1)
{
   cwidth.n=NULL; cwidth.u=NULL;
   if (is.data.frame(obj))  # If data.frame return a vector of widths
     {
       for (j in 1:ncol(obj))
          { cw <- char.width1(obj[,j], frmt=frmt, cx=cx)
            cwidth.n <- c(cwidth.n, cw$cwidth.n); cwidth.u <- c(cwidth.u, cw$cwidth.u);
          }
     }
   else # Otherwise simple call
     {
       cw <- char.width1(obj, frmt=frmt, cx=cx)
       cwidth.n <- cw$cwidth.n; cwidth.u <- cw$cwidth.u;
     }

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}

### CHARACT.HEIGHT ###
# Returns height in units(inches) based on a gpar setting
char.height <- function(charact = "A", frmt, cx=1)
{

  cheight.u <- convertHeight(grobHeight(textGrob(charact, gp=gpar(fontfamily = frmt$fontfamily,
                                                                  fontsize = frmt$fontsize,
                                                                  fontface = frmt$fontface,
                                                                  cex = cx
                                                                  ))), "inches", valueOnly =TRUE)
   return(cheight.u)
}

# ==================================== #
#               CHAR.DIM               #
# ==================================== #
#  PURPOSE:  Given the table object and style object, will return the maximum width of columns, the height of characters in the units specified by user
char.dim <- function(obj,     # Table Object
                     style,
                     cx=1
                     )
{
  cw.grp.u <- 0; cw.grp.n <- 0;
  cw.lbl.u <- 0; cw.lbl.n <- 0;
  cw.row.u <- NULL; cw.row.n <- NULL;
  cw.bdy.u <- NULL; cw.bdy.n <- NULL;
  ch.bdy   <- NULL; ch.grp   <- NULL;  ch.lbl <- NULL; ch.main<- NULL; ch.ftn<-NULL;

  ### Character Width for each section of Table ###
  for (l.i in 1:length(obj))
    {
      if (!is.null(obj[[l.i]]$group))  # Group
        {
         cw.grp   <- char.width(obj[[l.i]]$group, frmt=style$frmt.grp, cx=cx)
         cw.grp.u <- c(cw.grp.u, cw.grp$cwidth.u)
         cw.grp.n <- c(cw.grp.n, cw.grp$cwidth.n)
        }
      if (!is.null(obj[[l.i]]$label))  # Label
        {
         cw.lbl   <- char.width(obj[[l.i]]$label, frmt=style$frmt.lbl, cx=cx)
         cw.lbl.u <- c(cw.lbl.u, cw.lbl$cwidth.u)
         cw.lbl.n <- c(cw.lbl.n, cw.lbl$cwidth.n)
        }
      # Body
      cw.bdy   <- char.width(obj[[l.i]]$bdy, frmt=style$frmt.bdy, cx=cx)
      cw.bdy.u <- rbind(cw.bdy.u, cw.bdy$cwidth.u)
      cw.bdy.n <- rbind(cw.bdy.n, cw.bdy$cwidth.n)
    }
  # Define the width of one character so constants may be added for white space buffering between columns
  cw.bdy1 <- char.width("A", frmt=style$frmt.bdy, cx=cx)
  cw.grp1 <- char.width("A", frmt=style$frmt.grp, cx=cx)
  cw.lbl1 <- char.width("A", frmt=style$frmt.lbl, cx=cx)


  # Recycle Handling for defining buffer as a vector
  if (length(style$frmt.bdy$buf) < ncol(obj[[1]]$bdy)) {style$frmt.bdy$buf <- rep(style$frmt.bdy$buf, ncol(obj[[1]]$bdy))}

  # Column Names (First Logical Row)
  # Take the Column Name and span.beg, which indicates which column it is in, transpose and feed back to char width
  cw.col   <- char.width(as.data.frame(t(obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, c("cname", "span.beg")])), frmt=style$frmt.col, cx=cx)
  # For multiple lines
  cw.col$cwidth.u <- tapply(cw.col$cwidth.u, obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, "span.beg"], max)
  cw.col$cwidth.n <- tapply(cw.col$cwidth.n, obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, "span.beg"], max)
  # Take max accross all simple tables, and column names, so that all printed tables will have the same column width
  cw.bdy.u <- apply(rbind(cw.col$cwidth.u, cw.bdy.u), 2, max) + style$frmt.bdy$buf*cw.bdy1$cwidth.u # Correction to account for user specified buffer, it is a constant
  cw.bdy.n <- apply(rbind(cw.col$cwidth.n, cw.bdy.n), 2, max) + style$frmt.bdy$buf*cw.bdy1$cwidth.n

  a <- cw.bdy.u
  colhead.xloc <- cumsum(c(.5*a, 0) + c(0, .5*a))

  # Adjust starting location for columns based on justify
  if (style$justify == "right")
    {
      a <- c(cw.bdy.u, .5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u)
      a[1] <- a[1]-.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u
    }
  if (style$justify == "center")
    {
      a <- cw.bdy.u
      a <- c(.5*a, 0) + c(0, .5*a)
    }
  if (style$justify == "left")
    {
     a <- c(.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u,cw.bdy.u)
     a[length(a)] <- a[length(a)]-.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u
    }
#    bdy.xloc <- cumsum(bdy.xloc)
    bdy.xloc <- cumsum(a)
  if (!is.null(obj[[l.i]]$group))
    {
      cw.grp.u=max(cw.grp.u) + style$frmt.grp$buf*cw.grp1$cwidth.u;
      cw.grp.n=max(cw.grp.n) + style$frmt.grp$buf*cw.grp1$cwidth.n;
    };
  if (!is.null(obj[[l.i]]$label))
    { #        Max Characters +  Buffer                           +  Indention Under Group
      cw.lbl.u=max(cw.lbl.u) + style$frmt.lbl$buf*cw.lbl1$cwidth.u + style$indent*cw.lbl1$cwidth.u;
      cw.lbl.n=max(cw.lbl.n) + style$frmt.lbl$buf*cw.lbl1$cwidth.n + style$indent*cw.lbl1$cwidth.n;
    }
  # Take the max of group and label
   cw.row.u=max(cw.grp.u, cw.lbl.u); cw.row.n=max(cw.grp.n, cw.lbl.n);


  ### Character Height for each section of Table ###
  ch.bdy  <- char.height("A", frmt=style$frmt.bdy, cx=cx)
  ch.grp  <- char.height("A", frmt=style$frmt.grp, cx=cx)
  ch.lbl  <- char.height("A", frmt=style$frmt.lbl, cx=cx)
  ch.col  <- char.height("A", frmt=style$frmt.col, cx=cx)
  ch.main <- char.height("A", frmt=style$frmt.main, cx=cx)
  ch.ftn  <- char.height("A", frmt=style$frmt.ftn, cx=cx)
  ch.row  <- max(ch.bdy, ch.grp, ch.lbl); # Probably what is going to be used, the max character height
  ### Linespace in Relative Units ###
  linespace.bdy  <- ch.row*style$frmt.bdy$linespace   # Number of characters for lines space specified by user, multiplied by the max unit for character height
  linespace.col  <- ch.col*style$frmt.col$linespace
  linespace.main <- ch.main*style$frmt.main$linespace
  linespace.ftn  <- ch.ftn*style$frmt.ftn$linespace

  lbl.indent <- style$indent*cw.lbl1$cwidth.u;

  return(list(cw.grp.u=cw.grp.u, cw.grp.n=cw.grp.n, # Max Column width for group, label, max(group, label) as row, and body
              cw.lbl.u=cw.lbl.u, cw.lbl.n=cw.lbl.n,
              cw.row.u=cw.row.u, cw.row.n=cw.row.n,
              cw.bdy.u=cw.bdy.u, cw.bdy.n=cw.bdy.n,
              ch.bdy = ch.bdy, ch.grp = ch.grp, ch.lbl = ch.lbl, ch.main = ch.main, ch.row=ch.row, ch.col=ch.col, ch.ftn=ch.ftn,# Column Height for each section of table
              bdy.xloc = bdy.xloc, # Location for Columns
              colhead.xloc=colhead.xloc, # Location for column hiearchy
              lbl.indent=lbl.indent,
              linespace.bdy=linespace.bdy, # Lines space between rows in units
              linespace.col=linespace.col,
              linespace.main=linespace.main,
              linespace.ftn=linespace.ftn
              ))
}

# ==================================== #
#               SIZE.SIMP              #
# ==================================== #
# PURPOSE:  Measures size of properties of the simple table
size.simp <- function(tbl.obj, char.dim.obj, pagelayout, loc.y)
{
  # see if label and group were defined for number of columns to allocate: want to be 0 or 1
  nlbl  <- min(1, length(tbl.obj$label))
  ngrp  <- min(1, length(tbl.obj$group))
  nmain <- max(c(0, tbl.obj$main.nrw), na.rm=T)
  nfoot <- max(c(0, tbl.obj$footnote.nrw+.5), na.rm=T)
  main.height     <- nmain*char.dim.obj$linespace.main
  footnote.height <- nfoot*char.dim.obj$linespace.ftn

  # To calculate the height of the current table Number of rows for each section times character height
  tbl.height    <- tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.m  <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.mf <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy + footnote.height
  tbl.width     <- char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[length(char.dim.obj$bdy.xloc)]

  line.height     <- c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.bdy, tbl.obj$nrw) + rep(char.dim.obj$linespace.footnote, nfoot))
  cumheight.bdy <- cumsum(rep(char.dim.obj$linespace.bdy, tbl.obj$nrw)) + sum(c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.footnote, nfoot)))

  fitspage <- cumheight.bdy <= (loc.y - pagelayout$margins[3])
  newpage=FALSE;
  tablebreak.dx = NULL;
  nothingfits <- FALSE

  if (sum(fitspage) < tbl.obj$nrw) # table will not fit on page
    {
      if (sum(fitspage)==0) {nothingfits <- TRUE}
      else if (!is.null(tbl.obj$group[1]))  # If grouping was defined keep groups together when possible
        {
          fitspage.grp.dx <- which(fitspage & tbl.obj$lastofgroup)
          if      (length(fitspage.grp.dx) > 0)
            {
             tablebreak.dx <- fitspage.grp.dx[length(fitspage.grp.dx)];
             newpage=TRUE
            }
          #else if (call2) {tablebreak.dx <- which(fitspage)[1]; newpage=TRUE}  #  After page is resetM, IF group is too, large break rule
          else  {nothingfits <- TRUE} #  Reset page and see if group works
        }
      else # Assume no group and fit rows to page
        {
          tablebreak.dx <- which(fitspage)[length(which(fitspage))]
          newpage <- TRUE
        }
      }

  return(list(tbl.height=tbl.height,           # table height (EXlcuding main/footnote)
              tbl.height.m=tbl.height.m,       # table height Including footnote
              tbl.height.mf=tbl.height.mf,     # table height Including main and footnote
              tbl.width=tbl.width,
              line.height=line.height,         # The height of each line in a vector,
              cumheight.bdy=cumheight.bdy,     # The height of each line in a vector
              nothingfits=nothingfits,
              tablebreak.dx=tablebreak.dx      # Rownumber for table to be split upon
              ))
}

# ==================================== #
#               FITPAGE                #
# ==================================== #
# PURPOSE:  Calcuates character expansion to apply accross table presentation so that table fits within margins
fitpage <- function(size.simp,
                    pagelayout,
                    fit.width=FALSE,
                    fit.height=FALSE,
                    fit=FALSE,
                    cex.0            # default cex
                    )
{
  cex.width  <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)]))/size.simp$tbl.width
  cex.height <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)]))/size.simp$tbl.height.mf
  cntrbuf.horz <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)])-size.simp$tbl.width)/2
  cntrbuf.vert <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)])-size.simp$tbl.height.mf)/2

  if (fit.width&fit.height|fit) {cex.fit<-min(cex.width, cex.height)}
  else if (fit.width)           {cex.fit<-cex.width}
  else if (fit.height)          {cex.fit<-cex.height}
  else                          {cex.fit<-cex.0}

  return(list(cex.fit=cex.fit,cntrbuf.horz=cntrbuf.horz, cntrbuf.vert=cntrbuf.vert))
}

# PURPOSE: Given an index, breaks table into multiple tables
tablebreak <- function(tbl.obj.l,  # list of table objects
                       cur.tbl,    # Index to current table
                       dx          # Row to break table on
                       )
{
  tbl.obj1 <- tbl.obj2 <- NULL;
  # These structures stay the same
  tbl.obj1[[1]]$main <-  tbl.obj2[[1]]$main <-tbl.obj.l[[cur.tbl]]$main
  tbl.obj1[[1]]$main.nrw <- tbl.obj2[[1]]$main.nrw <- tbl.obj.l[[cur.tbl]]$main.nrw
  tbl.obj1[[1]]$cond.txt <- tbl.obj2[[1]]$cond.txt <- tbl.obj.l[[cur.tbl]]$cond.txt
  tbl.obj1[[1]]$ncl <- tbl.obj2[[1]]$ncl <- tbl.obj.l[[cur.tbl]]$ncl
  tbl.obj1[[1]]$colhead <- tbl.obj2[[1]]$colhead <- tbl.obj.l[[cur.tbl]]$colhead
  tbl.obj1[[1]]$nrw.colhead <- tbl.obj2[[1]]$nrw.colhead <- tbl.obj.l[[cur.tbl]]$nrw.colhead
  tbl.obj1[[1]]$footnote <- tbl.obj2[[1]]$footnote <- tbl.obj.l[[cur.tbl]]$footnote
  tbl.obj1[[1]]$footnote.nrw <- tbl.obj2[[1]]$footnote.nrw <- tbl.obj.l[[cur.tbl]]$footnote.nrw

  # These row strucutres need to be adjusted
  dx1 <- 1:dx
  dx2 <- (dx+1):(tbl.obj.l[[cur.tbl]]$nrw)

  tbl.obj1[[1]]$nrw <- dx
  tbl.obj2[[1]]$nrw <- tbl.obj.l[[cur.tbl]]$nrw - dx

  if (!is.null(tbl.obj.l[[cur.tbl]]$group[1]))
    {
      tbl.obj1[[1]]$group       <-  tbl.obj.l[[cur.tbl]]$group[dx1]
      tbl.obj2[[1]]$group       <-  tbl.obj.l[[cur.tbl]]$group[dx2]
      tbl.obj1[[1]]$newgrp      <-  tbl.obj.l[[cur.tbl]]$newgrp[dx1]
      tbl.obj2[[1]]$newgrp      <-  tbl.obj.l[[cur.tbl]]$newgrp[dx2]
      tbl.obj1[[1]]$lastofgroup <-  tbl.obj.l[[cur.tbl]]$lastofgroup[dx1]
      tbl.obj2[[1]]$lastofgroup <-  tbl.obj.l[[cur.tbl]]$lastofgroup[dx2]
      tbl.obj1[[1]]$newgrp.dx   <-  tbl.obj.l[[cur.tbl]]$newgrp.dx[tbl.obj.l[[cur.tbl]]$newgrp.dx <= dx]
      tbl.obj2[[1]]$newgrp.dx   <-  tbl.obj.l[[cur.tbl]]$newgrp.dx[tbl.obj.l[[cur.tbl]]$newgrp.dx > dx] - dx
      tbl.obj1[[1]]$newgrp.qdx  <-  tbl.obj.l[[cur.tbl]]$newgrp.qdx[tbl.obj.l[[cur.tbl]]$newgrp.dx <= dx]
      tbl.obj2[[1]]$newgrp.qdx  <-  tbl.obj.l[[cur.tbl]]$newgrp.qdx[tbl.obj.l[[cur.tbl]]$newgrp.dx > dx] - max(tbl.obj1[[1]]$newgrp.dx)
    }
  else
    {
      tbl.obj1[[1]]$group <-  tbl.obj2[[1]]$group <- NULL;
      tbl.obj1[[1]]$newgrp <- tbl.obj2[[1]]$newgrp <- NULL;
      tbl.obj1[[1]]$lastofgroup <- tbl.obj2[[1]]$lastofgroup <- NULL;
      tbl.obj1[[1]]$newgrp.dx <- tbl.obj2[[1]]$newgrp.dx <-  NULL;
      tbl.obj1[[1]]$newgrp.qdx <-  tbl.obj2[[1]]$newgrp.qdx <-  NULL;
    }
  if (!is.null(tbl.obj.l[[cur.tbl]]$label[1]))
    {
      tbl.obj1[[1]]$label <-  tbl.obj.l[[cur.tbl]]$label[dx1]
      tbl.obj2[[1]]$label <-  tbl.obj.l[[cur.tbl]]$label[dx2]
    }
  else {tbl.obj1[[1]]$label <- tbl.obj2[[1]]$label <- NULL}

  if (length(tbl.obj.l[[cur.tbl]]$lblescp.dx))
    { DD <- 'A'
      # [[!!!insert code here!!!]]
    }
  tbl.obj1[[1]]$bdy <-  tbl.obj.l[[cur.tbl]]$bdy[dx1, ]
  tbl.obj2[[1]]$bdy <-  tbl.obj.l[[cur.tbl]]$bdy[dx2, ]
  if (!is.null(tbl.obj.l[[cur.tbl]]$row.hl$dx) )
    {
       tbl.obj1[[1]]$row.hl$dx <-  tbl.obj.l[[cur.tbl]]$row.hl$dx[tbl.obj.l[[cur.tbl]]$row.hl$dx <= dx]
       tbl.obj2[[1]]$row.hl$dx <-  tbl.obj.l[[cur.tbl]]$row.hl$dx[tbl.obj.l[[cur.tbl]]$row.hl$dx > dx] - dx
       tbl.obj1[[1]]$row.hl$col <- tbl.obj2[[1]]$row.hl$col <- tbl.obj.l[[cur.tbl]]$row.hl$col
    }
  else {tbl.obj1[[1]]$row.hl$dx <- tbl.obj2[[1]]$row.hl$dx <- tbl.obj1[[1]]$row.hl$col <- tbl.obj2[[1]]$row.hl$col <- NULL;}

  ## Reconstruct List of Table objects ##
  if (length(tbl.obj.l)==1)              {tbl.obj.l <- c(tbl.obj1, tbl.obj2)}
  else if (cur.tbl == 1)                 {tbl.obj.l <- c(tbl.obj1, tbl.obj2, tbl.obj.l[2:length(tbl.obj.l)])}
  else if (cur.tbl == length(tbl.obj.l)) {tbl.obj.l <- c(tbl.obj.l[1:(cur.tbl-1)], tbl.obj1, tbl.obj2)}
  else                                   {tbl.obj.l <- c(tbl.obj.l[1:(cur.tbl-1)], tbl.obj1, tbl.obj2, tbl.obj.l[(cut.tbl+1):(length(tbl.obj.l))])}
 return(tbl.obj.l)
}

# ========================================= #
#              DPRINT.TABLE                 #
# ========================================= #
# PURPOSE:  Manages page layout and prints multiple tables by dispatching DPRINT.SIMP
dprint.table <- function(fmla=NULL,         # Formula interface to define table structure
                         data,              # Input Data.frame
                         label = NULL,      # label & group are characters identifying columns that define the simple table structure
                         group = NULL,
                         regx=NA,           # Regular Expression to take off of colnames, designed to break unwanted tiebreakers for legal data.frame columnnames
                         style=NULL,        # List of graphical parameters or table format options returned from STYLE(),
                         main=NA,           # Table Title, Vector of Strings
                         footnote=NA,       # Footnote, Vector of Strings
                         dtype="rgraphics", # Device type, currently available "rgraphics", "portrait", "landscape"
                         pg.dim=NULL,       # If custom page dimensions define a vector of c(width, height) units. Custom page dimensions over ride dtype
                         margins=NULL,      # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches. Other forms include a constant for all margins and c(top/bottom,left/right)
                         showmargins=FALSE, # Display margins on page in red
                         row.hl=NULL,       # Conditional highlight; row.hl(dx=, col=)
                         fit.width=FALSE,   # logical. If TRUE, Forces the table to fit the table horizontally
                         fit.height=FALSE,  # logical. If TRUE, Forces the table to fit the table vertically, if conditional variabled defined will force the first table to fit one page and same ratio will be applied to all tables
                         fit=FALSE,         # logical. If TRUE, Force the table to fit both horizontally&vertically (smaller of two cex calulcations)
                         newpage=FALSE,     # logical. If TRUE, when the device runs out of spacE a new page will automatically be started
                         center.horz=FALSE, # Center table horizontally.
                         center.vert=FALSE, # Center table vertically, should only be used on one table
                         center=FALSE,      # Center both vertically and horizontally. These should probably not be used with the fit.* parameters
                         f.hdr=NULL,        # Pass Function for printing header
                         f.ftr=NULL,        # Pass Function for printing Footer
                         pagenum=NULL,      # Starting page number, will override pagenumber from lastcall
                         lastcall=NULL      # Last call from dprint
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

# ========================================= #
#                DPRINT.SIMP                #
# ========================================= #
# PURPOSE:  Print the simplest version of a table, the formula and table structure have already been defined here.
dprint.simp <- function(tbl.obj,          # Simple Table object
                        init = c(0, 6.9), # Starting Position of Table
                        style,
                        char.dim.obj,
                        size.simp.obj
                        )
{
  # For sections Table Title, Colum heading, body, footnote, I want to update a global parameter that tells where the last section left off. So updates are contaiend it one variable
  y.loc <- y.rem <- init[2]
  # If missing  make 0
  main.nrw <- ifelse(is.na(tbl.obj$main.nrw), 0, tbl.obj$main.nrw)

  # Calculate how big the table is going to be
  #size.simp.obj <- size.simp(tbl.obj, char.dim)

  # --------------------------- #
  ###    Print Table Title    ###
  # --------------------------- #
  if (!is.na(tbl.obj$main.nrw))
    { # If there is conditinal text, append it to the last position of main vector
      if (!is.null(tbl.obj$cond.txt)) {tbl.obj$main[tbl.obj$main.nrw] <- paste(tbl.obj$main[tbl.obj$main.nrw], tbl.obj$cond.txt, sep=": ")}
      for (main.dx in 1:tbl.obj$main.nrw)
        {#                                                              a little extra space for lines background
          y.loc <- y.rem - char.dim.obj$linespace.main*(main.dx-.5) + .25*char.dim.obj$linespace.col
          grid.text(tbl.obj$main[main.dx],
                    just = "left",
                    gp = gpar(fontfamily = style$frmt.main$fontfamily, fontface = style$frmt.main$fontface,  fontsize = style$frmt.main$fontsize, cex = style$cex, col = style$frmt.main$col),
                    x = unit(init[1], "inches"), y = unit(y.loc, "inches"))

        }
      y.rem <- y.rem - char.dim.obj$linespace.main*(main.dx-.5) # update overall y location, without column addition
    }
  else {y.rem <- y.rem + .5*char.dim.obj$linespace.col}  # When no title, column headings will start excatly on top margin

  ### Conditional row highlight ###
  # Must be before any text or borders for correct effect
  for (y.dx in 1:tbl.obj$nrw)
    { #
      y.loc <-  y.rem - char.dim.obj$linespace.col*tbl.obj$nrw.colhead - char.dim.obj$linespace.bdy*y.dx
      if (!is.null(row.hl))
        {  if (y.dx %in% tbl.obj$row.hl$dx)
            { grid.rect(x = unit(init[1],"inches"), y = unit(y.loc, "inches"),
                        height = unit(char.dim.obj$linespace.bdy, "inches"), width = unit(size.simp.obj$tbl.width, "inches"),
                        just = "left", gp = gpar(col = tbl.obj$row.hl$col, fill = tbl.obj$row.hl$col, fontsize = style$frmt.bdy$fontsize, cex = style$cex))
            }
        }
    }
  # --------------------------- #
  ###  Prints Column Headings ###
  # --------------------------- #
  ### Column Heading Background ###
  # X, From begining left of table with width of table
  # Y, Centered around middle of column heading with height as the number of of columnheading rows ***.125
  grid.rect(x = unit(init[1],"inches"), y = unit(init[2]-(main.nrw)*char.dim.obj$linespace.main-.5*char.dim.obj$linespace.col*(tbl.obj$nrw.colhead-.25), "inches"),
            height = unit(char.dim.obj$linespace.col*(tbl.obj$nrw.colhead+.125), "inches"), width = unit(size.simp.obj$tbl.width, "inches"),
            just = "left", gp = gpar(col = style$frmt.col$bg, fill = style$frmt.col$bg, fontsize = style$frmt.col$fontsize, cex = style$cex))
  for (col.dx in 1:nrow(tbl.obj$colhead))
    {
      col.y.loc <- y.rem - char.dim.obj$linespace.col*tbl.obj$colhead[col.dx, "row"]
      # X location and justification is different for column hiearchies than for rest of table
      if (tbl.obj$colhead[col.dx, "col.logical.row"]==1) # Lowest column name descriptor
        { # This uses the bdy.xloc
          col.x.loc <- init[1] + char.dim.obj$cw.row.u +
                        char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.beg"]] +
                        .5*(char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.end"]]-char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.beg"]])
          j <- style$justify
        }
      else # Column Hiearchy printing and formatting
        { # This uses the colhead.xloc
          col.x.loc <- init[1] + char.dim.obj$cw.row.u +
                        char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]] +
                        .5*(char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]]-char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]])
         j <- "center"
        ### Column Hiearchy Border ###
        if (style$frmt.colh$bty%in%c("o")) # border entire column
          {
            dborder(c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]]-.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.beg"]],
                        col.y.loc+.625*char.dim.obj$linespace.col),
                    c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]]+.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.end"]],
                       init[2]-size.simp.obj$tbl.height.m-.25*char.dim.obj$linespace.col), frmt=style$frmt.colh)
          }
        if (style$frmt.colh$bty=="_") # underline column hiearchy name
          {
            dborder(c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]],#-.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.beg"]],
                        col.y.loc+.5*char.dim.obj$linespace.col),
                    c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]],#+.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.end"]],
                      col.y.loc-.5*char.dim.obj$linespace.col), frmt=style$frmt.colh)
          }

        }
        ### PRINT COLUMN NAMES ###
        grid.text(tbl.obj$colhead[col.dx, "cname"],
            just = j,
            gp = gpar(fontfamily = style$frmt.col$fontfamily, fontface = style$frmt.col$fontface,  fontsize = style$frmt.col$fontsize, cex = style$cex, col = style$frmt.col$col),
            x = unit(col.x.loc, "inches"), y = unit(col.y.loc, "inches"))
    }
  # Update Global y.loc variable                                  ***.25
  y.rem <- y.rem - char.dim.obj$linespace.col*(tbl.obj$nrw.colhead + .25)
  ### Column Heading Border ###
  dborder(c(init[1], init[2]-main.nrw*char.dim.obj$linespace.main + .25*char.dim.obj$linespace.col),
          c(init[1]+size.simp.obj$tbl.width, y.rem-.375*char.dim.obj$linespace.col), frmt=style$frmt.col)

  ### Prints Table and Row Attributes ###
  for (y.dx in 1:tbl.obj$nrw)
    { #
      y.loc <- y.rem - char.dim.obj$linespace.bdy*y.dx

      ### Line Separator ###
      if (y.dx != tbl.obj$nrw)
        {  dborder(c(init[1], y.loc-.5*char.dim.obj$linespace.bdy),
                   c(init[1]+size.simp.obj$tbl.width, y.loc-.5*char.dim.obj$linespace.bdy), frmt=style$frmt.bdy)}
          if (!is.null(tbl.obj$group[1]))  # If group is defined
            {
              if (!is.na(tbl.obj$group[y.dx]))  # If element is NOT NA
              { ### GROUP ###
                grid.text(tbl.obj$group[y.dx],
                          just = "left",
                          gp = gpar(fontfamily = style$frmt.grp$fontfamily, fontface = style$frmt.grp$fontface, fontsize = style$frmt.grp$fontsize, cex = style$cex, col = style$frmt.grp$col),
                          x = unit(init[1], "inches"), y = unit(y.loc, "inches"))
              }
              if (tbl.obj$lastofgroup[y.dx] & (y.dx != tbl.obj$nrw))  # If last row of group, and not row of table (so doesn't conflict with table border)
              {
                ### Group Separator ###
                dborder(c(init[1], y.loc-.5*char.dim.obj$linespace.bdy),
                        c(init[1]+size.simp.obj$tbl.width, y.loc-.5*char.dim.obj$linespace.bdy), frmt=style$frmt.grp)
              }

            }
          if (!is.null(tbl.obj$label[1]))  # If label is defined
            {
              if (!is.na(tbl.obj$label[y.dx])) # If element is NOT NA
                { ### LABEL ###
                  grid.text(tbl.obj$label[y.dx],
                            just = "left",
                            gp = gpar(fontfamily = style$frmt.bdy$fontfamily, fontface = style$frmt.bdy$fontface, fontsize = style$frmt.bdy$fontsize, cex = style$cex, col = style$frmt.bdy$col),
                            x = unit(init[1]+char.dim.obj$lbl.indent, "inches"), y = unit(y.loc, "inches"))
                }
            }
      for (x.dx in 1:tbl.obj$ncl)
        { #        Initial Location + Label&Group Length + Column Width
          x.loc <- init[1] + char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[x.dx]
          ## Prints all elements in table, Suppressing NAs ##
          if (!is.na(tbl.obj$bdy[y.dx , x.dx]))
            {
              grid.text(tbl.obj$bdy[y.dx , x.dx],
                        just = style$justify,
                        gp = gpar(fontfamily = style$frmt.bdy$fontfamily, fontface = style$frmt.bdy$fontface, fontsize = style$frmt.bdy$fontsize, cex = style$cex, col = style$frmt.bdy$col),
                        x = unit(x.loc, "inches"), y = unit(y.loc, "inches"))
            }
        }
    }
  y.rem <- y.loc

  ### Entire Border ###
  # CORD1: left margin, and top margin and correcting for table title, adjust for extra column space in col header
  # CORD2: left margin+table width, and top margin - table height, adjust for extra column space in col header
  dborder(c(init[1], init[2]-main.nrw*char.dim.obj$linespace.main+.25*char.dim.obj$linespace.col), c(init[1]+size.simp.obj$tbl.width, init[2]-size.simp.obj$tbl.height.m-.25*char.dim.obj$linespace.col), frmt=style$frmt.tbl)

  ### Print Table Footnote ###
  if (!is.na(tbl.obj$footnote.nrw))
    {
      for (footnote.dx in 1:tbl.obj$footnote.nrw)
        {
          y.loc <- y.rem - char.dim.obj$linespace.ftn*.25 - char.dim.obj$linespace.ftn*footnote.dx
          grid.text(tbl.obj$footnote[footnote.dx],
                    just = "left",
                    gp = gpar(fontfamily = style$frmt.ftn$fontfamily, fontface = style$frmt.ftn$fontface,  fontsize = style$frmt.ftn$fontsize, cex = style$cex, col = style$frmt.ftn$col),
                    x = unit(init[1], "inches"), y = unit(y.loc, "inches"))

        }
      y.rem <- y.loc # update overall y location.
    }

   # Update Global y.loc variable
   y.rem <- y.loc
   return(
     list(cord1=c(init[1], init[2]),
          cord2=c(init[1] + char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[length(char.dim.obj$bdy.xloc)], y.rem-.5*char.dim.obj$linespace.bdy)
          ))
}

# ################# FOOTER HEADER FUNCTIONS ############################# #
# ========================================= #
#                   HDR                     #
# ========================================= #
#  PURPOSE:  A function for printing headers.  Provides a template for defining custom header functions that may be passed to the function
hdr <- function(txt1,
                frmt1=frmt(fontfamily="", fontface="bold", fontsize=20, col="blue", bty="_", lwd=2, linespace=1),
                txt2=NA, # oPTIONAL, see as being used for indicating CONFIDENTIAL document justified right, in upper right hand corner
                frmt2=frmt(fontfamily="", fontface="bold", fontsize=16, col="red", bty="_", lwd=2, linespace=1),
                pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)) # Header is printed in reference to the margins of this function
                )
{

  txt1.struct <- vector.struct(txt1)
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  y.rem <- y.loc <- pagelayout.obj$page.height
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }
  grid.lines(x = unit(c(pagelayout.obj$margins[2], pagelayout.obj$page.width-pagelayout.obj$margins[4]), "inches"),
             y = unit(pagelayout.obj$page.height-pagelayout.obj$margins[3], "inches"),
             gp = gpar(col = frmt1$lcol, fontsize = frmt1$fontsize, cex = frmt1$cex, lwd = frmt1$lwd, lty=frmt1$lty),
                draw = TRUE)

  if (!is.na(txt2))
  {
    txt2.struct <- vector.struct(txt2)
    ch2         <- char.height("A", frmt=frmt2, cx=1)
    linespace2  <- frmt2$linespace*ch2
    y.loc <- y.rem
    for (txt2.dx in 1:txt2.struct$vctr.nrw)
      {
        y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace2)*txt2.dx
        grid.text(txt2.struct$vctr[txt2.dx],
                  just = "right",
                  gp = gpar(fontfamily = frmt2$fontfamily, fontface = frmt2$fontface, fontsize = frmt2$fontsize, cex = frmt2$cex, col = frmt2$col),
                  x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))

      }
  }
}

# ========================================= #
#                   FTR                     #
# ========================================= #
#  PURPOSE:  A function for printing footers.  Provides a template for defining custom footer functions that may be passed to the function
ftr <- function(txt1,
                frmt1=frmt(fontfamily="", fontface="plain", fontsize=8, col="black", linespace=.75),
                page=TRUE,
                date = TRUE,
                pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)), # Header is printed in reference to the margins of this function
                pgtxt2 = "page", # Text appended to page number
                pagenum=NULL
                  )
{

  txt1.struct <- vector.struct(txt1)
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  if (is.null(pagenum)) {print('test');pagenum <- eval.parent(pagenum, n = 1)}

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (date)
  {
    y.loc <- y.rem -  (linespace1)*txt1.dx
    grid.text(format(Sys.Date(), "%m/%d/%Y"),
              just = "right",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (page)
  {
    y.loc <- y.rem -  2*(linespace1)
    grid.text(paste(pgtxt2, "\n", pagenum, sep=""),
              just = "center",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width/2, "inches"), y = unit(y.loc, "inches"))
  }

}

### Classess/Methods of DPRINT will go HERE  ###
#
dprint <- function(...)
  { dprint.table(...)}

as.tabular.lm <- function(lm.obj, rnd=c(2,2,2, 3))
{
    lm.s    <- summary(lm.obj)
    # Coefficitent table
    coef.d       <- as.data.frame(lm.s$coefficients)
    #coef.d       <- round(coef.d, rnd)

    coef.d[,1] <- format(round(coef.d[,1], rnd[1]), digits=ceil(log(max(coef.d[,1]))), nsmall=rnd[1])
    coef.d[,2] <- format(round(coef.d[,2], rnd[2]), digits=ceil(log(max(coef.d[,2]))), nsmall=rnd[2])
    coef.d[,3] <- format(round(coef.d[,3], rnd[3]), digits=ceil(log(max(coef.d[,3]))), nsmall=rnd[3])
    #coef.d[,4] <- format(round(coef.d[,4], rnd[4]), digits=ceil(log(max(coef.d[,4]))), nsmall=rnd[4])
    coef.d[,4] <- format.pval(coef.d[,4], digits=rnd[4], eps=10^(-1*rnd[4]))
    coef.d$param <- rownames(coef.d)
    coef.d       <- as.data.frame(coef.d)
    # Summary stats 1
    smry.stat1 <-
      rbind(data.frame(Stat="R-Squared",               Estimate=round(lm.s$r.squared, 4),     DF=NA,         P.value=NA),
            data.frame(Stat="Adjusted R-Squared",      Estimate=round(lm.s$adj.r.squared, 4), DF=NA,         P.value=NA),
            data.frame(Stat="Residual Standard Error", Estimate=round(lm.s$sigma, 2),         DF=lm.s$df[2], P.value=NA),
            data.frame(Stat="F-Statistic",             Estimate=round(lm.s$fstatistic[1], 1), DF=lm.s$df[1], P.value=round(1-pf(lm.s$fstatistic[1], lm.s$df[1], lm.s$df[2]), 4))
            )

    #smry.stat1[,2] <- format(round(smry.stat1[,2], rnd[2]), digits=ceil(log(max(smry.stat1[,2]))), nsmall=rnd[2])
    smry.stat1[!is.na(smry.stat1[,4]),4] <- format.pval(smry.stat1[!is.na(smry.stat1[,4]),4], digits=rnd[4], eps=10^(-1*rnd[4]))

    return(list(coef.d=coef.d, smry.stat1=smry.stat1))
}


#"Regression Analysis on Freeny's Quarterly Revenue Data")
dprint.lm <- function(lm.obj)
{
  tabular.obj <- as.tabular.lm(lm.obj)
  hh <- expression(hdr("Linear Model Summary", frmt1=frmt(fontfamily="", fontface="bold", fontsize=14, linespace=1, lwd=2),
                      pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(.3, .1)))
                  )
  dd <- dprint.table(param~., tabular.obj[[1]], main="Parameter Estimates",
                     style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")),
                     margins=c(.5, .25),
                     f.hdr=hh
                     )
  dprint.table(Stat~., data=tabular.obj[[2]], lastcall=dd, main="Model Summary Statistics",
               style=style(justify="right", frmt.col=frmt(bg="cornflowerblue", col="white")))
}



