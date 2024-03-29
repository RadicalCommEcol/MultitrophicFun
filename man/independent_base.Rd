% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/independent_base.R
\name{independent_base}
\alias{independent_base}
\title{complete vector space base}
\usage{
independent_base(P1, noise.threshold)
}
\arguments{
\item{P1}{square matrix}

\item{noise.threshold}{vector, lower and upper limit to the noise term}
}
\value{
square matrix
}
\description{
complete the base of the vector space for a square matrix, by
adding independent columns to it. New columns are denoted by I_1 to I_N
}
