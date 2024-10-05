#' Recalculate relative quantities for a given experiment
#'
#' @param x A `pcr` or `data.frame`
#' @param relative_sample A sample to set others relative to (eg
#'   `my_dmso_sample`)
#' @param group Character vector to specify columns used for subsetting data, where each
#'   group will have its own relative_sample
#' @param control_probe Character. `target_name` to serve as endogenous control.
#' @param ... Arguments passed to respective method
#'
#' @return An object of same class as `x`
#' @export
pcr_rq <- function(x, relative_sample, group = NULL, control_probe = NULL, ...) {
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
pcr_rq.pcr <- function(x, relative_sample, group = NULL, control_probe = NULL, ...) {

  pcr_clean <- tidy_if_not(x)

  if (is.null(control_probe)) {
    control_probe <- x$footer["endogenous_control"]
  }

  pcr_clean |>
    mop::scrub() |>
    pcr_rq.data.frame(relative_sample, group, control_probe) |>
    mop::as_pcr()
}

#' @export
#' @rdname pcr_rq
pcr_rq.data.frame <- function(x, relative_sample, group = NULL, control_probe = NULL, ...) {
  control_probe <- get_control_probe(x, control_probe)
  x <- make_group_col(x, group)
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
      .by = c("target_name", "sample_name", "..group")
    )

  minimal_data <- ct_summaries |>
    dplyr::select(
      "ct_mean", "ct_sd", "target_name", "sample_name", "rep", "..group"
    )

  minimal_data <- tidyr::unite(
    minimal_data, "sample_group", c("sample_name", "..group"), remove = FALSE
  )

  minimal_data <- tidyr::unite(
    minimal_data, "target_group", c("target_name", "..group"), remove = FALSE
  )

  w_rq <- minimal_data |>
    tapply(~sample_group, \(x) calc_dct(x, control_probe)) |>
    array2DF() |>
    dplyr::select(-1) |>
    tapply(~target_group, \(x) calc_rq(x, relative_sample)) |>
    array2DF() |>
    dplyr::select(-1)


  dplyr::left_join(used_wells, w_rq, by = c("sample_name", "target_name", "..group"))
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

make_group_col <- function(data, group) {
  if (is.null(group)) {
    data$..group <- 1
    return(data)
  }
  stopifnot(all(group %in% colnames(data)))
  tidyr::unite(data, "..group", dplyr::all_of(group), remove = FALSE)
}

calc_dct <- function(x, control_probe) {
  control <- dplyr::filter(x, .data$target_name == control_probe)
  x$delta_ct <- x$ct_mean - control$ct_mean
  x$delta_ct_sd <- sqrt(x$ct_sd^2 + control$ct_sd^2)
  x$delta_ct_se <- x$delta_ct_sd / sqrt(x$rep)
  x$df <- max(1, x$rep + control$rep - 2)
  x$t <- stats::qt(0.05 / 2, x$df, lower.tail = FALSE)
  x
}

calc_rq <- function(x, relative_sample) {
  relative <- dplyr::filter(x, .data$sample_name == relative_sample)
  x$delta_delta_ct <- x$delta_ct - relative$delta_ct
  x$rq <- 2^-(x$delta_delta_ct)
  x$rq_min <- 2^-(x$delta_delta_ct + x$t * x$delta_ct_se)
  x$rq_max <- 2^-(x$delta_delta_ct - x$t * x$delta_ct_se)
  x
}
