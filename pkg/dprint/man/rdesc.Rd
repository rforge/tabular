\name{rdesc}
\alias{rdesc}
\alias{rbin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate Descriptives
%%  ~~function to do ... ~~
}
\description{Simulate a Data Frame Containing Descriptive Statistics
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rdesc(n.grp = 8, n.lvls = 5, n.grp2 = NULL, rnd = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.grp}{Number of hierarchies describing the rows of the data frame
%%     ~~Describe \code{n.grp} here~~
}
  \item{n.lvls}{Number of levels nested within each hierarchy
%%     ~~Describe \code{n.lvls} here~~
}
  \item{n.grp2}{A second level of hierachry describing the rows of the data frame
%%     ~~Describe \code{n.grp2} here~~
}
  \item{rnd}{logical.  Round columns to two decimals, consequently turns columns into character format
%%     ~~Describe \code{rnd} here~~
}
}
\details{
rdesc creates a data frame representing descriptives statistics of continuous variables and rbin for binomial
%%  ~~ If necessary, more details than the description above ~~
}
\value{data.frame
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Carlin
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
table1   <- rdesc(4, 5)        # Numeric
table1f <- rdesc(4, 5, rnd=T) # Rounded and pretty format so values are character
table1a <- table1; table1b<-table1;
table1a$group2 <- 1; table1b$group2 <- 2;
table2 <- rbind(table1a, table1b)
table2a <- table2; table2b<-table2

table2a$group3 <- "Zebra"; table2b$group3 <- "Elephant";
table3 <- rbind(table2a, table2b)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
