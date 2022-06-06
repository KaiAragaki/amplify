#' View sample plating layout
#'
#' @param tidy_pcr an output from the `pcr_tidy` function
#' @param fill character. A column in `tidy_pcr` used to use to fill the geom_tiles
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#'
#' @examples
#' system.file("extdata", "untidy-pcr-example.xls", package = "amplify") |>
#'   pcr_tidy() |>
#'   pcr_plate_view()

pcr_plate_view <- function(pcr, fill = target_name) {
  pcr <- tidy_if_not(pcr)
  gp::gp_plot(pcr$data, {{fill}})
}
