Fci <-
function(cil, ciu, mn=NA, digits=2, nsmall=2, NAmiss=TRUE)
{
  if (is.na(mn)) {f <- paste("(", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  else{f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  if (NAmiss) { f[is.na(cil)] <- ""}
  f
}

