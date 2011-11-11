\name{fmla.rhs}
\alias{fmla.rhs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula Interface: Right Hand Side
%%  ~~function to do ... ~~
}
\description{Parses Right hand side of formula
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fmla.rhs(f, span, Rnl = list(Rn.o = list(), Rn.n = NULL, rn.i = 0), byvars = list(byvars1 = NULL, byvars2 = NULL, byvars.i = 0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{span}{
%%     ~~Describe \code{span} here~~
}
  \item{Rnl}{
%%     ~~Describe \code{Rnl} here~~
}
  \item{byvars}{
%%     ~~Describe \code{byvars} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fmla.inter}}
\code{\link{fmla.rhs}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(f,
                  span, # There are two types of calls, when TRUE returns the spanning text, does not when FALSE
                  Rnl=list(Rn.o=list(), Rn.n=NULL, rn.i=0),
                  byvars=list(byvars1=NULL, byvars2=NULL, byvars.i=0)
                  )
 {
  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0;
  colon.op  = FALSE  # uses colon operator
  Rn.op     = FALSE  # uses Rn function (rename)
  cond.op   = FALSE  # uses | operator
  period.op = FALSE  # uses . operator to indicate all
  I.op      = FALSE  # Some type of other function should be kept as expression
  for (tm in fl)
    {
      i=i+1
      if (i==1)
        {
          rl[[i]] = tm
          if (tm == ".")  {period.op = TRUE;}
          if (tm == ":")  {colon.op = TRUE}
          if (tm == "Rn")
            {Rn.op = TRUE;
             # Store old name(expression) with new name and update rename index
             Rnl$rn.i<-Rnl$rn.i+1
             Rnl$Rn.o[[Rnl$rn.i]] <- fl[[2]]
             Rnl$Rn.n <- c(Rnl$Rn.n, fl[[3]])
             }
          if (tm == "|")
          {
            cond.op = TRUE
            rl[[i]] = as.symbol("+"); # Replace symbol so model.frame will keep column in data.frame
            byvars$byvars.i=byvars$byvars.i+1;
            if (byvars$byvars.i==1) {byvars$byvars1 <-  all.vars(fl[[3]])}
            if (byvars$byvars.i==2)
            {
              byvars$byvars2 <- byvars$byvars1
              byvars$byvars1 <- all.vars(fl[[3]])
              if (byvars$byvars1==".") {byvars$byvars1<-NULL}
            }
          }
          if (sum(colon.op, Rn.op, cond.op, period.op)==0) {I.op<-TRUE}
        }
      if (i > 1)
        { # If elements of formula are still functinos call again
          if (typeof(tm) == "language")
           {
             rhs.obj <-  fmla.rhs(tm, span=span, Rnl=Rnl, byvars=byvars)
             rl <- c(rl, rhs.obj$rl)
             Rnl = rhs.obj$Rnl
             byvars = rhs.obj$byvars
           }
          else rl[[i]] = tm
        }
   }
  # When span = FALSE, remove spanning text from formula
  if (!span) {if (colon.op) {return(list(rl=rl[[3]], Rnl=Rnl, byvars=byvars))}}
  # Rename, second element is variables being renamed
  if (Rn.op)    {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  if (cond.op)  {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  # Any other function/operator not defined here, treat as is
  if (I.op)     {return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))}

  # Last Call of Recursion
  if (period.op)
  {
    return(list(rl=as.symbol(rl[[1]]), Rnl=Rnl, byvars=byvars))
  }
  return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
