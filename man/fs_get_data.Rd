% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fs_get_data.R
\name{fs_get_data}
\alias{fs_get_data}
\title{Get and process ziff data for fisheriescape project.}
\usage{
fs_get_data(
  years = NULL,
  species.sought = NULL,
  nafo = NULL,
  gclass = NULL,
  gearcode = NULL
)
}
\arguments{
\item{years}{Which years? Required}

\item{species.sought}{Which species sought statac code? Required}

\item{nafo}{Which NAFO divisions? Case insensitive}

\item{gclass}{Which gear class?}

\item{gearcode}{Which gear code?}
}
\description{
Get and process ziff data for fisheriescape project.
}
\examples{
#fs_get_data(years = 2017, species.sought = 705, nafo=c('4r'), gclass='1', gearcode=c('62','99'))
}
