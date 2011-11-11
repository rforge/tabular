\name{tbl.struct.simp}
\alias{tbl.struct.simp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple Table Structre
%%  ~~function to do ... ~~
}
\description{Simple Table Structre
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tbl.struct.simp(data, label = NULL, group = NULL, main = NA, footnote = NA, colnames.obj = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame
%%     ~~Describe \code{data} here~~
}
  \item{label}{name of column containing row labels
%%     ~~Describe \code{label} here~~
}
  \item{group}{name of column containing hieriarchy labels for the row names
%%     ~~Describe \code{group} here~~
}
  \item{main}{table title
%%     ~~Describe \code{main} here~~
}
  \item{footnote}{foot note
%%     ~~Describe \code{footnote} here~~
}
  \item{colnames.obj}{colnames object defined by colnames.struct
%%     ~~Describe \code{colnames.obj} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Simple table structure
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
\code{\link{colnames.struct}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data, label = NULL, group = NULL, main=NA, footnote=NA, colnames.obj=NULL)
  {
    newgrp       <- NULL # boolean, when new group starts
    newgrp.dx    <- NULL # index, when new group starts
    newgrp.gdx   <- NULL # Unique identifier for group
    lastofgroup  <- NULL # boolean, when last level of group
    ngrp         <- NULL # number of groups
    lblescp      <- rep(FALSE, nrow(data)) # boolean, when label should be escaped
    lblescp.dx   <- NULL # index, when label should be escaped
    cond.txt     <- NULL # Text from conditional group
    ### Row Label Hierarchy ###
    # If no label or group vector defined, tbl is original data
    if (is.null(label) & is.null(group))
      {bdy <- data}
    else
      { # If label or group vector defined in data frame, than remove
        bdy <- data[ , -which(colnames(data) \%in\% c(label, group))]
        if (!is.data.frame(bdy))
        {
         bdy <- as.data.frame(bdy)
         colnames(bdy) <- colnames(data)[-which(colnames(data) \%in\% c(label, group))]
         }
        if(!is.null(label))
          { label <- paste("  ",as.character(data[ , label]), sep="")}
        if(!is.null(group))
          { # Place an empty character on group so that text is not on top of the border, this is the best place since will be inbedded in on nhcar calcs
            group <- paste("  ", as.character(data[ ,group]), sep="")
            # Indicate when the group changes
            l.g          <- length(group)
            prev.grp     <- group[1:(l.g-1)]
            newgrp       <- c(TRUE, group[2:l.g] != prev.grp)
            lastofgroup <- c(newgrp[-1], TRUE)
            newgrp.dx    <- which(newgrp)
            ngrp         <- length(newgrp.dx)
            newgrp.gdx   <- cumsum(newgrp)
            # Handle label excape character
            group[!newgrp] <- NA
            if (!is.null(label))
              { lblescp    <- label == "  <<"         # boolean, when label escape character is present
                lblescp.dx <- which(lblescp)  # index, when escape character is present
                if (length(lblescp.dx) > 0)
                  {
                    label[lblescp.dx] <-NA         # Assign NA so it does not print a label for this one
                    dummy.sort <- 1:nrow(bdy)
                    dummy.sort[lblescp.dx] <- -1
                  }
              }
          }
      }


    ### Column Name Structure ###
    if (is.null(colnames.obj)) {colnames.obj <- colnames.struct(colnames(bdy))}
    nrw.colhead  <- max(colnames.obj$row)
    nrw <- nrow(bdy); ncl <- ncol(bdy);

    ### Main Structure ###
    main.obj <- vector.struct(main)
    ### Footnote Structure ###
    footnote.obj <- vector.struct(footnote)

    return(list(main=main.obj$vctr,           # Title
                main.nrw=main.obj$vctr.nrw,   # Number of rows title will take
                cond.txt=cond.txt,
                nrw = nrw, ncl = ncl,         # Number of rows & Columns in table
                colhead=colnames.obj,         # Structure to drive printing of column heading, created by COLNAMES.STRUCT
                nrw.colhead=nrw.colhead,      # Number of rows for column heading
                ngrp=ngrp, group = group, label = label, newgrp = newgrp, newgrp.dx=newgrp.dx,newgrp.gdx=newgrp.gdx,
                lastofgroup=lastofgroup,      # Indiates the last level of group
                lblescp=lblescp, lblescp.dx=lblescp.dx,
                bdy = bdy,
                footnote=footnote.obj$vctr,           # Footnote
                footnote.nrw=footnote.obj$vctr.nrw   # Number of rows footnote will take
              ))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
