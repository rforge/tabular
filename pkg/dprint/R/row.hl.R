row.hl <-
function(dx,
                   col="yellow")
  {
   if(is.logical(dx)) {dx<-which(dx)}
   return(list(dx=dx, col=col))
  }

