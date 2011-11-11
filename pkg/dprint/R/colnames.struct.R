colnames.struct <-
function(col.names, linebreak=TRUE)
{
  # look for escape character indicating that a grouping scheme exists for column names
  col.grp.dx      <- grep(":", col.names)
  col.grp.split   <- strsplit(col.names, ":")
  orig.names      <- unlist(lapply(col.grp.split, FUN=function(x) {x[length(x)]}))
  lst.names       <- lapply(col.grp.split, FUN=function(x) {x[-length(x)]}) # group names (if any) after removing origninal column names
  lnames          <- length(orig.names)
  # Logical row keeps column hiercahies together, col.row indicates whether line breaks have been performed
  column.heading  <- data.frame(cname = orig.names, col.logical.row = rep(1, lnames), col.row=1, span.beg=1:lnames, span.end=1:lnames, stringsAsFactors =F)
  r.i             <- nrow(column.heading)
  cname.hierc.df  <- list.to.df(lst.names)

  if(!is.null(cname.hierc.df))
    {
      for (hier.i in ncol(cname.hierc.df):1)
        {
          d.i <- consect.struct(cname.hierc.df[, hier.i])
          n.d <- nrow(d.i$consec.begend)
          column.heading[r.i+ (1:n.d), "col.logical.row"] <- ncol(cname.hierc.df)-hier.i+2
          column.heading[r.i+ (1:n.d), "col.row"] <-1
          column.heading[r.i+(1:n.d), c("cname", "span.beg", "span.end")] <- d.i$consec.begend
          r.i <- nrow(column.heading)
        }
    }
  column.heading$cname  <- kill.multiregx(column.heading$cname, "`") #  Remove backhashes
  if (linebreak) # Can be called in two different places with different assumptions whether the line break has been added yet
    {
      column.heading        <- colnames.linebreak(column.heading)        #  Account for line breaks in column names
      column.heading        <- colnames.row(column.heading)              #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
    }
  column.heading
}

