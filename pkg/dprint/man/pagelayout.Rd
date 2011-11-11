\name{pagelayout}
\alias{pagelayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pagelayout(dtype = "rgraphics", margins = NULL, pg.dim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dtype}{
%%     ~~Describe \code{dtype} here~~
}
  \item{margins}{
%%     ~~Describe \code{margins} here~~
}
  \item{pg.dim}{
%%     ~~Describe \code{pg.dim} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dtype="rgraphics", # Device type, currently available "rgraphics", "portrait", "landscape"
                       margins=NULL,   # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
                       pg.dim=NULL     # If custom page dimensions define a vector of c(width, height) units
                       )
{
  if (dtype=="rgraphics")
   {
      page.height <- 7; page.width  <- 7
      if (is.null(margins)) {margins = rep(.2,4)}
   }
  if (dtype == "portrait")
    {
     page.height = 11;  page.width  = 8.5;
     if (is.null(margins)) {margins = c(1, 1, 1.25, 1)}
    }
  if (dtype == "landscape")
    {
     page.height = 8.5; page.width  = 11;
     if (is.null(margins)) {margins = c(1, 0.5, 1, 0.5)}
    }

  # Custom page dimensions over ride dtype
  if (!is.null(pg.dim)) {pg.dim <- rep(pg.dim, 2)[1:2]; page.width<-pg.dim[1]; page.height<-pg.dim[2];}
  #  Handles different ways of defining margins common that left=right, and top =bottom, so c(1, .5) is shorter
  if (length(margins) < 4) {margins <- rep(margins, floor(4/length(margins)))[1:4]}
  # Top left, and bottom right cordinates
  cord.tl <- c(margins[2], page.height-margins[3]) # Top Left
  cord.br <- c(page.width-margins[4], margins[1]) # Bottom right

  return(list(margins=margins, page.height=page.height, page.width=page.width,cord.tl=cord.tl, cord.br=cord.br))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
