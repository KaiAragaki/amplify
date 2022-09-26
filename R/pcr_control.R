#' Calculate Delta Ct mean based on given control probe
#'
#' @param x A `pcr` or `data.frame` object
#' @param control_probe A probe to be used as an endogenous control (eg GAPDH)
#'
#' @return An object with class the same as input
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' system.file("extdata", "untidy-pcr-example.xls", package = "amplify") |>
#'   read_pcr() |>
#'   pcr_control("GAPDH")
pcr_control <- function(x, control_probe) {
  UseMethod("pcr_control")
}

#' @rdname pcr_control
#' @export
pcr_control.pcr <- function(x, control_probe, ...) {
  x |>
    tidy_if_not() |>
    mop::scrub() |>
    pcr_control(control_probe) |>
    mop::as_pcr()
}

#' @rdname pcr_control
#' @export
pcr_control.data.frame <- function(x, control_probe, ...) {
  x |>
    dplyr::mutate(endogenous_control = control_probe) |>
    dplyr::group_by(.data$target_name, .data$sample_name) |>
    dplyr::mutate(ct_mean = mean(.data$ct),
                  ct_sd   = stats::sd(.data$ct),
                  rep     = dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::nest(sample_nest = c("ct", "well", ".row", ".col", "well_position",
                                "baseline_start", "baseline_end")) |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::mutate(delta_ct     = .data$ct_mean - .data$ct_mean[.data$target_name == endogenous_control],
                  delta_ct_sd  = sqrt(.data$ct_sd^2 + .data$ct_sd[.data$target_name == endogenous_control]^2),
                  delta_ct_se  = .data$delta_ct_sd/sqrt(.data$rep),
                  df           = max(1, .data$rep + .data$rep[.data$target_name == endogenous_control] - 2),
                  t            = stats::qt(.05/2, .data$df, lower.tail = FALSE)) |>
    tidyr::unnest(.data$sample_nest) |>
    dplyr::ungroup()
}
