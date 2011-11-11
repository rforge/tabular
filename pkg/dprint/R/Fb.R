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

