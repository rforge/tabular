fmla.lhs <-
function(f, grp=NULL, lvl=NULL, rhs.lpad=FALSE) {

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

