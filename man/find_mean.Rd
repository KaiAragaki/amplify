% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcr_lib.R
\name{find_mean}
\alias{find_mean}
\title{Find mean of ct without putative outlier}
\usage{
find_mean(df)
}
\arguments{
\item{df}{a data.frame containing a numeric column named ct}
}
\value{
a list, with the mean and sd of ct without the putative outlier
}
\description{
Find mean of ct without putative outlier
}
\details{
if there are fewer than three rows, or if all values are NA, the
function will simply return the mean and standard deviation without
removing a putative outlier.
}
\keyword{internal}
