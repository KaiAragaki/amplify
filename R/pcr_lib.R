#' Calculate library PCR concentrations
#'
#' @param tidy_pcr a dataset run that has been run through `pcr_tidy()`
#' @param dil_factor the factor to which the libraries were diluted for pcr
#'
#' @return a list, containing the original dataframe, data on the standards, and data on the samples.
#' @export
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

#' Generate visual library prep pcr quality control report
#'
#' @param lib_calc_pcr an output from `pcr_lib_calc`
#' @return a list
#' @export
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

find_mean <- function(df){
  if(nrow(df) >= 3 & !all(is.na(df$ct))) {
    hc <- df$ct |>
      stats::dist() |>
      stats::hclust()
    possible_outlier <- hc$merge[nrow(df) - 1, 1] |>
      abs()
    no_po <- df$ct[-possible_outlier]
    no_po_mean <- mean(no_po)
    no_po_sd <- stats::sd(no_po)
    list(no_po_mean = no_po_mean, no_po_sd = no_po_sd)
  } else {
    no_po_mean <- mean(df$ct)
    no_po_sd <- stats::sd(df$ct, na.rm = T)
    list(no_po_mean = no_po_mean, no_po_sd = no_po_sd)
  }
}


pcr_lib_qc_report <- function(pcr_lib_qc) {

  report <- system.file("rmd", "lib-qc.Rmd", package = "amplify")

  # If there's a user supplied filename, use that. If not, use current date and time.
  if (is.null(report_name)) {
    report_name <- paste0(gsub(":", "-", gsub("\\s", "_", Sys.time())),"_report.html")
  } else {
    report_name <- paste0(report_name, ".html")
  }

  # Generate report
  rmarkdown::render(report,
                    output_dir = "./pcr_qc_reports",
                    output_file = report_name,
                    params = list(
                      out = make_outlier_plot(dat),
                      samp = table_samples,
                      conc = make_conc_plot(table_samples),
                      slope = make_slope_plot(standards, slope_text)))
}
