#' Print Table to Graphics Device (dprint)
#'
#' drpint method (to be implemented)
#' 
#' @param data Input data.frame of class "data.frame"
#' @param ... see \code{\link{dprint.data.frame}}
#' @export
#' 
#' @author Carlin Brickner
dprint <-
function(data, ...)
{ UseMethod("dprint", data)
  #dprint.data.frame(...)
  }

