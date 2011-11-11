colnames.row <-
function(colnames.obj)
  {
     max.row <- tapply(colnames.obj$col.row, list(colnames.obj$col.logical.row), function(x) max(x, na.rm=T))
     d.max.row <- data.frame(max.row)
     d.max.row$col.logical.row <- rownames(d.max.row)
     d.max.row$lag.max.row.adj <- c(0, d.max.row$max.row[-nrow(d.max.row)]-1)
     colnames.obj <- merge(colnames.obj, d.max.row, by ="col.logical.row")
     colnames.obj$row <- colnames.obj$col.logical.row + colnames.obj$col.row -1 + colnames.obj$lag.max.row.adj
     colnames.obj$row <- max(colnames.obj$row) - (colnames.obj$row-1)
     colnames.obj
  }

