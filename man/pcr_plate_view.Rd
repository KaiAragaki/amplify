% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcr_plate_view.R
\name{pcr_plate_view}
\alias{pcr_plate_view}
\title{View sample plating layout}
\usage{
pcr_plate_view(pcr, fill = target_name)
}
\arguments{
\item{fill}{character. A column in \code{tidy_pcr} used to use to fill the geom_tiles}

\item{tidy_pcr}{an output from the \code{pcr_tidy} function}
}
\value{
a ggplot
}
\description{
View sample plating layout
}
\examples{
system.file("extdata", "untidy-pcr-example.xls", package = "amplify") |>
  read_pcr() |>
  pcr_plate_view()
}
