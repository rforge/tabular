#' Format Binomial
#'
#' Inline formula formatting of columns representing binomial summary statistics
#' 
#' @param prop column name representing a proportion
#' @param r column name representing number of binomial events observed
#' @param n column name representing number of observations
#' @param digits.p digits for proportion presentation. see format
#' @param nsmall.p nsmall for proportion presentation.
#' @param digits.n number of digits for n presentation
#' @export
Fb <-
function (prop=NULL, r=NULL, n=NULL, digits.p=4, nsmall.p=2, digits.n=6)
  {

    if (!is.null(n) & !is.null(r))
      {f <- paste(format(round(100*r/n, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else if (!is.null(n))
      {f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else
      {{f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% ", sep="")}}
    f
  }

