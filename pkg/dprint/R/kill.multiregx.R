kill.multiregx <-
function(string, # string
                           regx # Perl Regular expression
                           )
{

  string <- sub(regx, '', string, perl = TRUE)
  g.spc <- grep(regx, string)
  if(length(g.spc) > 0)   { string <- kill.multiregx(string, regx) }
  return(string)
}

