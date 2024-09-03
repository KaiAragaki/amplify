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
#' read_pcr(dat_path) |>
#'   pcr_rq("U6D1")
#'
#' # Can also be run after using pcr_control:
#' read_pcr(dat_path) |>
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
  control_probe <- get_control_probe(x, control_probe)

  # Minimally required columns
  stopifnot(
    all(c(".row", ".col", "ct", "target_name", "sample_name") %in% colnames(x))
  )

  # Remove variables that we will recalculate
  x <- x |>
    dplyr::select(
      -dplyr::any_of(
        c("ct_mean", "ct_sd", "rep", "delta_ct", "delta_ct_sd", "delta_ct_se",
          "delta_delta_ct", "df", "t", "rq", "rq_min", "rq_max")
      )
    )

  used_wells <- x |>
    dplyr::filter(!is.na(.data$target_name), !is.na(.data$sample_name))

  ct_summaries <- used_wells |>
    dplyr::summarize(
      ct_mean = mean(.data$ct, na.rm = TRUE),
      ct_sd = stats::sd(.data$ct, na.rm = TRUE),
      rep = dplyr::n(),
      .by = c("target_name", "sample_name")
    )

  minimal_data <- ct_summaries |>
    dplyr::select("ct_mean", "ct_sd", "target_name", "sample_name", "rep")

  calc_dct <- function(x) {
    control <- dplyr::filter(x, .data$target_name == control_probe)
    x$delta_ct <- x$ct_mean - control$ct_mean
    x$delta_ct_sd <- sqrt(x$ct_sd^2 + control$ct_sd^2)
    x$delta_ct_se <- x$delta_ct_sd / sqrt(x$rep)
    x$df <- max(1, x$rep + control$rep - 2)
    x$t <- stats::qt(0.05 / 2, x$df, lower.tail = FALSE)
    x
  }

  calc_rq <- function(x) {
    relative <- dplyr::filter(x, .data$sample_name == relative_sample)
    x$delta_delta_ct <- x$delta_ct - relative$delta_ct
    x$rq <- 2^-(x$delta_delta_ct)
    x$rq_min <- 2^-(x$delta_delta_ct + x$t * x$delta_ct_se)
    x$rq_max <- 2^-(x$delta_delta_ct - x$t * x$delta_ct_se)
    x
  }

  w_rq <- minimal_data |>
    tapply(~sample_name, calc_dct) |>
    array2DF() |>
    dplyr::select(-1) |>
    tapply(~target_name, calc_rq) |>
    array2DF() |>
    dplyr::select(-1)

  dplyr::left_join(used_wells, w_rq, by = c("sample_name", "target_name"))
}

get_control_probe <- function(df, control_probe) {
  if (is.null(control_probe)) {
    if (!"endogenous_control" %in% names(df)) {
      stop("`control_probe` is NULL and `x$endogenous_control` does not exist")
    }
    control_probe <- na.omit(unique(df$endogenous_control))
    if (length(control_probe) != 1)
      stop("length(unique(probes)) in `endogenous_control` != 1.")
  }
  control_probe
}
