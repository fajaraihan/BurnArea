% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_dnbr.R
\name{calculate_dnbr}
\alias{calculate_dnbr}
\title{Calculate Delta Normalized Burn Ratio (dNBR)}
\usage{
calculate_dnbr(nbr_pre, nbr_post)
}
\arguments{
\item{nbr_pre}{Raster object of pre-fire NBR}

\item{nbr_post}{Raster object of post-fire NBR}
}
\value{
Raster object of dNBR values or classified burn severity
}
\description{
This function calculates the difference between pre-fire NBR and post-fire NBR
to assess burn severity.
}
