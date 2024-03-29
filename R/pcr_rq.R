#' Recalculate relative quantities for a given experiment
#'
#' @param x A `pcr` or `data.frame`
#' @param relative_sample A sample to set others relative to (eg `my_dmso_sample`)
#' @param control_probe Character. `target_name` to serve as endogenous control.
#' @param ... Arguments passed to respective method
#'
#' @return An object of same class as `x`
#' @export
pcr_rq <- function(x, relative_sample, control_probe = NULL, ...) {
  UseMethod("pcr_rq")
}

#' @export
#' @rdname pcr_rq
#' @importFrom rlang .data
#'
#' @examples
#' dat_path <- system.file("extdata", "untidy-pcr-example.xls", package = "amplify")
#'
#' pcr_tidy(dat_path) |>
#'   pcr_rq("U6D1")
#'
#' # Can also be run after using pcr_control:
#' pcr_tidy(dat_path) |>
#'   pcr_control("GAPDH") |>
#'   pcr_rq("U6D1")
pcr_rq.pcr <- function(x, relative_sample, control_probe = NULL, ...) {

  pcr_clean <- tidy_if_not(x)

  if (is.null(control_probe)) {
    control_probe <- x$footer["endogenous_control"]
  }

  pcr_clean |>
    mop::scrub() |>
    pcr_rq.data.frame(relative_sample, control_probe) |>
    mop::as_pcr()
}

#' @export
#' @rdname pcr_rq
pcr_rq.data.frame <- function(x, relative_sample, control_probe = NULL, ...) {

  if (is.null(control_probe)) {
    control_probe <- x$endogenous_control[1]
  }

  x |>
    dplyr::group_by(.data$target_name, .data$sample_name) |>
    dplyr::mutate(ct_mean = mean(.data$ct, na.rm = TRUE),
                  ct_sd   = stats::sd(.data$ct, na.rm = TRUE),
                  rep     = dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::nest(sample_nest = c("ct", "well", ".row", ".col", "well_position",
                                "baseline_start", "baseline_end")) |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::mutate(delta_ct     = .data$ct_mean - .data$ct_mean[.data$target_name == control_probe],
                  delta_ct_sd  = sqrt(.data$ct_sd^2 + .data$ct_sd[.data$target_name == control_probe]^2),
                  delta_ct_se  = .data$delta_ct_sd/sqrt(.data$rep),  # Rep might not be the correct metric here
                  df           = max(1, .data$rep + .data$rep[.data$target_name == control_probe] - 2),
                  t            = stats::qt(.05/2, .data$df, lower.tail = FALSE)) |>
    dplyr::group_by(.data$target_name) |>
    dplyr::filter(!is.na(.data$sample_name)) |>
    dplyr::mutate(delta_delta_ct = .data$delta_ct - .data$delta_ct[.data$sample_name == relative_sample],
                  rq     = 2^-(.data$delta_delta_ct),
                  rq_min = 2^-(.data$delta_delta_ct + .data$t * .data$delta_ct_se),
                  rq_max = 2^-(.data$delta_delta_ct - .data$t * .data$delta_ct_se)) |>
    tidyr::unnest(.data$sample_nest) |>
    dplyr::ungroup()
}
