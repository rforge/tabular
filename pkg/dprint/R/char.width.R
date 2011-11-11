char.width <-
function(obj, frmt, cx=1)
{
   cwidth.n=NULL; cwidth.u=NULL;
   if (is.data.frame(obj))  # If data.frame return a vector of widths
     {
       for (j in 1:ncol(obj))
          { cw <- char.width1(obj[,j], frmt=frmt, cx=cx)
            cwidth.n <- c(cwidth.n, cw$cwidth.n); cwidth.u <- c(cwidth.u, cw$cwidth.u);
          }
     }
   else # Otherwise simple call
     {
       cw <- char.width1(obj, frmt=frmt, cx=cx)
       cwidth.n <- cw$cwidth.n; cwidth.u <- cw$cwidth.u;
     }

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}

