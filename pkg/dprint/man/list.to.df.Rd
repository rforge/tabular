\name{list.to.df}
\alias{list.to.df}
\title{List to Data.frame}
\usage{
  list.to.df(lst)
}
\arguments{
  \item{lst}{this is a list returned from string split}
}
\description{
  Convert a list of vectors, of varying length, to a data
  frame. Elements are NA where lengths of vectors are
  smaller than the length of the largest vector.
}

