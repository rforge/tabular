#' Column Names Line Break
#'
#' Inserts a line break in column names when the escape character [backslash] n is found.
#' 
#' @param colnames.obj column names object
#' @export
colnames.linebreak <-
function(colnames.obj  # Column Names Object creatd by COLNAMES.STRUCT
                              )
{
  grep.linebreak <- grep("\n",colnames.obj$cname)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak  <- strsplit(colnames.obj$cname[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak         <- length(strsplit.linebreak[[1]]) # store how many line breaks
       d.temp         <- colnames.obj[rep(grep.linebreak[1], n.linebreak), ] # Expand for each linebreak in this particular name
       d.temp$cname   <- strsplit.linebreak[[1]] # insert section of string that was delimitted by the string break character
       d.temp$col.row <- n.linebreak:1
       if (grep.linebreak[1] == 1)
        {  # Insert new expanded rows into orignal object (if row occurs at begining of data frame)
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind( d.temp, d.after)
        }
       else if (grep.linebreak[1] == nrow(colnames.obj))
        {  # Insert new expanded rows into orignal object (if row occurs at end of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          colnames.obj <- rbind(d.before,  d.temp)
        }
       else
        {  # Insert new expanded rows into orignal object (if row occurs in middle of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind(d.before,  d.temp, d.after)
        }
        grep.linebreak <- grep("\n",colnames.obj$cname)
        # Recursive Calls
        if (length(grep.linebreak) > 0) {colnames.obj <- colnames.linebreak(colnames.obj)}
    }
  colnames.obj
}

