#' Recalcuate standard slope of quantity vs Ct
#'
#' @param tidy_pcr a object that has been tidied by `tidy_pcr`
#'
#' @return a tibble with an updated `slope` column
#' @export
pcr_calc_slope <- function(tidy_pcr) {
  tidy_pcr$data$well_data$slope <- stats::lm(ct~log10(quantity), data = tidy_pcr$data$well_data)$coefficients[2] |> unname()
  tidy_pcr
}


tidy_if_not <- function(x) {
  if (!x$is_tidy) {
    rlang::inform("pcr is not tidy, tidying.")
    x <- mop::tidy_lab(x)
  }
  x
}
