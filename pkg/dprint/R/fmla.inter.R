fmla.inter <-
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

