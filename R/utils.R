#' Recalcuate standard slope of quantity vs Ct
#'
#' @param tidy_pcr a object that has been tidied by `tidy_pcr`
#'
#' @return a tibble with an updated `slope` column
#' @export
pcr_calc_slope <- function(tidy_pcr) {
  tidy_pcr$slope <- stats::lm(ct~log10(quantity), data = tidy_pcr)$coefficients[2] |> unname()
  tidy_pcr
}
