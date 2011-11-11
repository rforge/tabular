fmla.rhs <-
function(f,
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

