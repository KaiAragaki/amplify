#' Calculate library PCR concentrations
#'
#' @param tidy_pcr a dataset run that has been run through `pcr_tidy()`
#' @param dil_factor the factor to which the libraries were diluted for pcr
#'
#' @return a list, containing the original dataframe, data on the standards, and data on the samples.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy() |>
#'   pcr_lib_calc()

pcr_lib_calc <- function(tidy_pcr, dil_factor = 1000) {
  tidy_pcr |>
    tidyr::nest(replicates = c("well", "well_position", "ct", "quantity",
                               "well_row", "well_col",
                               dplyr::starts_with("prfdrop"),
                               dplyr::starts_with("baxrox"))) |>
    dplyr::group_by(.data$task) |>
    dplyr::arrange(.data$ct_mean) |>
    dplyr::mutate(standard_diff = .data$ct_mean - dplyr::lag(.data$ct_mean, default = .data$ct_mean[1]),
                  dil = 2^.data$standard_diff,
                  quant_actual = 6.8/cumprod(.data$dil),
                  dil = dplyr::if_else(.data$dil == 1, 0, .data$dil)) |>
    tidyr::unnest(cols = .data$replicates) |>
    dplyr::mutate(dil = dplyr::if_else(.data$task == "STANDARD", .data$dil, NA_real_),
                  standard_diff = dplyr::if_else(.data$task == "STANDARD", .data$standard_diff, NA_real_),
                  quant_actual = dplyr::if_else(.data$task == "STANDARD", .data$quant_actual, .data$quantity),
                  concentration = .data$quantity_mean * dil_factor)
}

#' Create library prep quality control data
#'
#' @param lib_calc_pcr an output from `pcr_lib_calc`
#' @return a list
#' @details While the output of this function on its own is can theoretically be
#'   used to gauge library quality, it is best used in conjunction with a
#'   function like `pcr_lib_calc_report`
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' dat_path <- system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy(pad_zero = TRUE) |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc()
#'

pcr_lib_qc <- function(lib_calc_pcr) {
  dat <- lib_calc_pcr |>
    dplyr::select(c("task", "sample_name", "quantity_mean", "concentration", "quantity",
                    "quant_actual", "dil", "slope", "efficiency", "r2", "ct"))

  standards <- dat |>
    dplyr::filter(.data$task == "STANDARD") |>
    dplyr::select(-c("sample_name", "quantity_mean", "concentration"))

  samples <- dat |>
    dplyr::filter(.data$task == "UNKNOWN") |>
    dplyr::select(-c("dil"))

  sample_summary <- samples |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::summarize(quantity_mean = mean(.data$quantity_mean),
                     concentration_mean = mean(.data$concentration))

  standard_summary <- standards |>
    dplyr::group_by(.data$quantity) |>
    dplyr::summarize(quantity_mean = mean(.data$quantity),
                     quant_actual = mean(.data$quant_actual),
                     dil = mean(.data$dil)) |>
    tidyr::pivot_longer(cols = c(.data$quantity_mean, .data$quant_actual))

  list(standards = standards,
       samples = samples,
       sample_summary = sample_summary,
       standard_summary = standard_summary,
       outliers = find_outliers(dat))
}

#' Mark outliers and determine means and standard deviation without them
#'
#' @param dat
#'
#' @return a tibble
#' @keywords internal
#'
#' @importFrom rlang .data

find_outliers <- function(dat) {
  dat |>
    tidyr::nest(reps = c("ct", "quantity", "quant_actual", "concentration")) |>
    dplyr::mutate(mean_wo_outlier = purrr::map(.data$reps, find_mean)) |>
    tidyr::unnest_wider(.data$mean_wo_outlier) |>
    tidyr::unnest(cols = c("no_po_mean", "no_po_sd", "reps")) |>
    dplyr::mutate(keep = dplyr::case_when(.data$no_po_mean - (3*.data$no_po_sd) < .data$ct & .data$no_po_mean + (3 * .data$no_po_sd) > .data$ct ~ TRUE,
                                          is.na(.data$no_po_sd) ~ TRUE,
                                          TRUE ~ NA),
                  sample_name = dplyr::case_when(is.na(.data$sample_name) & .data$quantity > 6.0000 ~ "1",
                                                 is.na(.data$sample_name) & .data$quantity > 0.6000 ~ "1:10",
                                                 is.na(.data$sample_name) & .data$quantity > 0.0600 ~ "1:100",
                                                 is.na(.data$sample_name) & .data$quantity > 0.0060 ~ "1:1000",
                                                 is.na(.data$sample_name) & .data$quantity > 0.0006 ~ "1:10000",
                                                 TRUE ~ .data$sample_name)) |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::mutate(adj_mean = mean(.data$keep * .data$ct, na.rm = TRUE),
                  adj_sd   = stats::sd(.data$keep * .data$ct, na.rm = TRUE),
                  z = (.data$ct-.data$adj_mean)/.data$adj_sd)
}

#' Find mean of ct without putative outlier
#'
#' @param df a data.frame containing a numeric column named ct
#'
#' @return a list, with the mean and sd of ct without the putative outlier
#' @details if there are fewer than three rows, or if all values are NA, the
#'   function will simply return the mean and standard deviation without
#'   removing a putative outlier.
#'
#' @keywords internal
find_mean <- function(df) {
  cts <- df$ct
  if (nrow(df) >= 3 & !all(is.na(cts))) {
    hc <- cts |> stats::dist() |> stats::hclust()
    possible_outlier <- hc$merge[nrow(df) - 1, 1] |> abs()
    cts <- cts[-possible_outlier]
  }
  no_po_mean <- mean(cts, na.rm = TRUE)
  no_po_sd <- stats::sd(cts, na.rm = TRUE)
  list(no_po_mean = no_po_mean, no_po_sd = no_po_sd)
}


#' Generate visual library prep pcr quality control report
#'
#' @param pcr_lib_qc output from `pcr_lib_qc`
#' @param report_path the name of the report as well as where it should be
#'   output. If NULL, it will export to a temp directory
#' @return The path to the report
#' @export
#' @examples
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy() |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc() |>
#'   pcr_lib_qc_report()

pcr_lib_qc_report <- function(pcr_lib_qc, report_path = NULL) {
  report <- system.file("rmd", "pcr-lib-qc.Rmd", package = "amplify")
  if (missing(report_path)) {
    report_path <- tempfile(pattern = paste(Sys.Date(), "pcr_lib_qc_report", sep = "_"),
                            fileext = ".html")
  }
  print(report_path)
  rmarkdown::render(report,
                    output_file = report_path,
                    params = list(data = pcr_lib_qc))
  report_path
}
