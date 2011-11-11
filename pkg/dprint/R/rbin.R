rbin <-
function(n.grp = 8, n.lvls = 5, n.grp2=NULL)
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

