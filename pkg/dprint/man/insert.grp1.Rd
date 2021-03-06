% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert.grp.R
\name{insert.grp1}
\alias{insert.grp1}
\title{Insert Values}
\usage{
insert.grp1(obj, dx, lblescp, group = FALSE, val = NA, dx.up = FALSE)
}
\arguments{
\item{obj}{vector or data frame}

\item{dx}{Index where NA is to be inserted}

\item{lblescp}{Label Escape, boolean}

\item{group}{indicates that this object is related to the grouping vector and the insetions must go after}

\item{val}{Values to be inserted}

\item{dx.up}{Bolean, If true adjust an index}
}
\description{
Inserts a value into record in tbl.struct objects to control for when empty rows should be inserted into the data structure.  NA's will be suppressed during presentation of the table.
}
