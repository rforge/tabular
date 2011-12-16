#' Format Continuous
#'
#' Inline formula formatting of columns representing summary statistics for Continuous data
#' 
#' @param mn column name representing a mean
#' @param std column name representing standard deviation (Variance, Margin of Error, etc.)
#' @param digits see format
#' @param nsmall see format
#' @param NAmiss present NA as white space
#' @export
Fc <-
function(mn, std, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(std, digits), digits=digits, nsmall=nsmall), ")", sep="")
  if (NAmiss) { f[is.na(mn)] <- ""}
  f
}

