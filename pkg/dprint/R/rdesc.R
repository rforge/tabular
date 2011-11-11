rdesc <-
function(n.grp = 8, n.lvls = 5, n.grp2=NULL, rnd=FALSE)
  {
    g  <- rep(LETTERS[1:n.grp], rep(n.lvls, n.grp))
    if (!is.null(n.grp2)) {g2 <- rep(c(1:n.grp2), rep(ceil(length(g)/n.grp2), n.grp2))}
    f  <- rep(letters[(26-n.lvls+1):26], n.grp)
    f <- paste(f,f,f,f,f,f,f,f,f, sep="")
    mn1   <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    mdn1  <- rnorm((n.lvls*n.grp), 2.3, 1.1)
    var1  <- rchisq((n.lvls*n.grp), 1.1)
    mn2   <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    mdn2  <- rnorm((n.lvls*n.grp), 2.5, 1.3)
    var2  <- rchisq((n.lvls*n.grp), 1.3)
    pval  <- runif((n.lvls*n.grp), max = .25)
    table <- data.frame(group = g, level = f, Mean1 = mn1, Median1= mdn1, Variance1 = var1, Mean2 = mn2, Median2 = mdn2, Variance2 = var2, "p-value" = pval)
    if (!is.null(n.grp2))  {table$group2 <- g2}
    if(rnd) {    table[ , 3:9] <- prettyNum(round(table[ , 3:9], 2), drop0trailing=FALSE)}
    else {table[ , 3:9] <- table[ , 3:9]}
    return(table)
  }

