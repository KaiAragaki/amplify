#' @export
#' @rdname well_data
well_data.pcr <- function(x, ...) {
  x$data$well_data
}

#' Recalcuate standard slope of quantity vs Ct
#'
#' @param tidy_pcr or data.frame object that has been tidied by `tidy_pcr`
#'
#' @return a tibble with an updated `slope` column
#' @export
pcr_calc_slope <- function(tidy_pcr) {
  if (inherits(tidy_pcr, "pcr")) tidy_pcr <- tidy_pcr$data$well_data
  tidy_pcr$slope <- stats::lm(ct ~ log10(quantity), data = tidy_pcr)$coefficients[2] |> unname()
  tidy_pcr
}

tidy_if_not <- function(x) {
  if (!x$is_tidy) {
    rlang::inform("pcr is not tidy, tidying.")
    x <- mop::tidy_lab(x)
  }
  x
}
