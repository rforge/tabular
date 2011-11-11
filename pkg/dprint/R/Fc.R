Fc <-
function(mn, std, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(std, digits), digits=digits, nsmall=nsmall), ")", sep="")
  if (NAmiss) { f[is.na(mn)] <- ""}
  f
}

