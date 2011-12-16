#' Insert Values Table Structure
#'
#' Dispatcher for insert.grp1. Inserts NA positions for tbl, label, and group structure from tbl.obj.
#'
#' @param tbl.obj
#' @export
insert.grp <-
function(tbl.obj)
{
  if (!is.null(tbl.obj$group))
   {
      tbl.obj$bdy         <- insert.grp1(tbl.obj$bdy, tbl.obj$newgrp.dx, tbl.obj$lblescp)
      if (!is.null(tbl.obj$label))
        {tbl.obj$label  <- insert.grp1(tbl.obj$label, tbl.obj$newgrp.dx, tbl.obj$lblescp)}
      tbl.obj$group       <- insert.grp1(tbl.obj$group, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T)
      tbl.obj$newgroup    <- insert.grp1(tbl.obj$newgrp, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T, val=FALSE)
      tbl.obj$lastofgroup <- insert.grp1(tbl.obj$lastofgroup, tbl.obj$newgrp.dx, tbl.obj$lblescp, val=FALSE)
      tbl.obj$nrw         <- tbl.obj$nrw + length(tbl.obj$newgrp.dx) - sum(tbl.obj$lblescp)
      if(!is.null(tbl.obj$row.hl$dx)) # Row Highlight
        {tbl.obj$row.hl$dx <- insert.grp1(tbl.obj$row.hl$dx, tbl.obj$newgrp.dx, tbl.obj$lblescp, FALSE,val=1, dx.up=TRUE)}
      # This must go last
      {tbl.obj$newgrp.dx <- which(tbl.obj$newgroup)}
   }
 return(tbl.obj)
}

