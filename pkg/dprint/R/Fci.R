#' Format Interval
#'
#' Inline formula formatting of columns representing lower and upper bound of an interval where the results are of the form 'mn (cil, ciu)'
#'
#' @param cil column name representing lower interval
#' @param ciu column name representing upper interval
#' @param mn column name representing a mean
#' @param digits see format
#' @param nsmall see format
#' @param NAmiss present NA as white space
#' @export
Fci <-
function(cil, ciu, mn=NA, digits=2, nsmall=2, NAmiss=TRUE)
{
  if (is.na(mn)) {f <- paste("(", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  else{f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  if (NAmiss) { f[is.na(cil)] <- ""}
  f
}

