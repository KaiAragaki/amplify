#' Plot qPCR results
#'
#' @param pcr a tidy `pcr` object
#'
#' @return a `ggplot`
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' system.file("extdata", "untidy-pcr-example.xls", package = "amplify") |>
#'   pcr_tidy() |>
#'   pcr_rq("RD1") |>
#'   pcr_plot()

pcr_plot <- function(pcr) {
  pcr$data |>
    dplyr::filter(!is.na(.data$sample_name)) |>
    dplyr::distinct(.data$target_name, .data$sample_name, .keep_all = T) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$sample_name, y = .data$rq, fill = .data$target_name)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$rq_min, ymax = .data$rq_max)) +
    ggplot2::facet_wrap(~.data$target_name, scales = "free") +
    ggplot2::scale_fill_viridis_d(begin = 0.1, end = 0.9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   axis.title.x = ggplot2::element_blank())
}
