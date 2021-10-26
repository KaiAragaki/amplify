#' Calculate library PCR concentrations
#'
#' @param tidy_pcr an output of `pcr_tidy`
#' @param dil_factor integer. The factor to which the libraries were diluted for pcr
#'
#' @return a `tibble`, containing the input columns as well as:
#' \itemize{
#'   \item{`standard_diff`} {The difference between the `ct_mean` of a standard and one step up in the dilution (ie more concentrated, lower Ct). The most concentrated dilution has a value of 0}
#'   \item{`dil`} {2^(`standard_diff`). The accuracy of this metric assumes that the efficiency of the PCR is 100%, which is likely good but not perfect!. In the case of the first standard, `dil` = 0}
#'   \item{`quant_actual`} {For standards, the presumed quantity of standard, calculated from `dil`. For samples, `quantity`}
#'   \item{`concentration`} {The concentration of library, before dilution}
#' }
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy(pad_zero = TRUE) |>
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
#' @return a named list with:
#' \itemize{
#'   \item{standards} {Data for individual standards, including calculated dilutions, given and calculated quantities, raw Ct, etc.}
#'   \item{samples} {Data for individual samples, including calculated concentrations, raw Ct, etc.}
#'   \item{sample_summary} {Summary statistics for samples grouped by replicates}
#'   \item{standard_summary} {Summary statistics for standards groupd by replicates}
#'   \item{outliers} {Data for individual samples and standards with and without their putative outliers (`po`) per replicate group}
#' }
#' @details While the output of this function on its own is can theoretically be
#'   used to gauge library quality, it is best used in conjunction with a
#'   function like `pcr_lib_calc_report`
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
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
    dplyr::group_by(sample_name) |>
    tidyr::nest() |>
    dplyr::mutate(mean_wo_outlier = purrr::map(.data$data, find_mean)) |>
    tidyr::unnest_wider(.data$mean_wo_outlier) |>
    tidyr::unnest(cols = c(data)) |>
    dplyr::mutate(keep = dplyr::case_when(.data$no_po_mean - (3*.data$no_po_sd) < .data$ct & .data$no_po_mean + (3 * .data$no_po_sd) > .data$ct ~ TRUE,
                                          is.na(.data$no_po_sd) ~ TRUE,
                                          TRUE ~ FALSE),
                  keep_temp = if_else(keep, keep, NA)) |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::mutate(adj_mean = mean(.data$keep_temp * .data$ct, na.rm = TRUE),
                  adj_sd   = stats::sd(.data$keep_temp * .data$ct, na.rm = TRUE),
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
  output_dir <- stringr::str_remove(report_path, "[^/]*$")
  output_file <- stringr::str_extract(report_path, "[^/]*$")
  rmarkdown::render(report,
                    output_dir = output_dir,
                    output_file = output_file,
                    params = list(data = pcr_lib_qc))
  report_path
}


#' Plot standard dilutions compared to a perfect dilution
#'
#' @param lib_qc Output of `pcr_lib_qc`
#'
#' @return a `ggplot`
#' @export
#' @importFrom ggplot2 element_blank aes
#' @details An optimal dilution will show blue and grey dots perfectly aligned.
#'   A plot with blue dots consistently lagging more behind the gray dots
#'   implies the dilutions are consistent, but less dilute than a 1:10 dilution.
#'   Likewise, a plot with blue dots that consistently outpace the gray dots
#'   more with each passing dot signifies consistently over-diluting the
#'   standards.
#'
#'   Samples are shown as red dots.
#'
#' @examples
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy() |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc() |>
#'   pcr_lib_qc_plot_dil()
pcr_lib_qc_plot_dil <- function(lib_qc) {

  #                            12.7 <- dilution_lines (geom_text)
  #      sample summary  _________________
  #             v        |      ^         | <- vert_lines (geom_segment)
  #       O .. ..  .     O      |         O <- standard_summary
  #       |______________|      |------------- dilution_lines
  #             10.3

  dilution_lines <- lib_qc$standard_summary |>
    dplyr::filter(.data$name == "quant_actual") |>
    dplyr::mutate(line_start = 1/.data$value,
                  line_end = dplyr::lag(.data$line_start),
                  dil = dplyr::lag(.data$dil),
                  y = rep_len(c(1.1, 0.9), 5),
                  y_text = rep_len(c(1.15, 0.85), 5)) |>
    dplyr::filter(!is.na(.data$line_end)) |>
    dplyr::rowwise() |>
    dplyr::mutate(mid = sqrt(.data$line_start * .data$line_end))

  vert_lines <-
    dplyr::tibble(x = c(dilution_lines$line_start, dilution_lines$line_end)) |>
    dplyr::arrange(.data$x) |>
    dplyr::mutate(y = rep(c(1.1, 0.9, 1.1, 0.9), each = 2),
                  yend = rep(c(1.05, 0.95, 1.05, 0.95), each = 2))

  lib_qc$standard_summary |>
    ggplot2::ggplot(aes(x = 1/.data$value, y = 1, color = .data$name)) +
    ggplot2::geom_point(size = 10, alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("#00AAAA", "#222222")) +
    ggplot2::geom_segment(data = dilution_lines,
                          aes(x = .data$line_start, xend = .data$line_end,
                              y = .data$y, yend = .data$y),
                          size = 1) +
    ggplot2::geom_segment(data = vert_lines,
                          aes(x = .data$x, xend = .data$x,
                              y = .data$y, yend = .data$yend),
                          color = "#00AAAA", size = 1) +
    ggplot2::geom_text(data = dilution_lines,
                       aes(label = round(.data$dil, 1),
                           y = .data$y_text,
                           x = .data$mid),
                       size = 8) +
    ggplot2::scale_x_log10() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   legend.position = "none") +
    ggplot2::coord_cartesian(ylim = c(0.8, 1.2)) +
    ggplot2::geom_point(data = lib_qc$sample_summary, aes(x = 1/.data$quantity_mean, y = 1), color = "red")

}


#' Plot mean centered samples without putative outliers
#'
#' @param lib_qc Output of `pcr_lib_qc`
#'
#' @return a `ggplot`
#' @export
#'
#' @details A sample is deemed an outlier if, upon its removal, it is more that
#'   3Z from the mean of the remaining. This boundary of +/-3Z is demarcated by
#'   the shaded area. Gray samples are outliers. Samples |Z| > 10 away are
#'   denoted by arrows (<<<) pointing in their direction as well as with their Z
#'
#' @examples
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy(pad_zero = TRUE) |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc() |>
#'   pcr_lib_qc_plot_outliers()
pcr_lib_qc_plot_outliers <- function(lib_qc) {
  lib_qc$outliers |>
    dplyr::group_by(.data$sample_name) |>
    dplyr::mutate(keep_logi = !is.na(.data$keep),
                  z = (.data$ct-.data$adj_mean)/.data$adj_sd,
                  overflow = abs(.data$z) > 10,
                  z_plot = dplyr::case_when(.data$z > 10 ~ 10,
                                            .data$z < -10 ~ -10,
                                            TRUE ~ .data$z),
                  label = dplyr::case_when(.data$z > 10 ~ paste(as.character(round(.data$z, 0)),">>>"),
                                           .data$z < -10 ~ paste("<<<", as.character(round(.data$z, 0))),
                                           TRUE ~ NA_character_)) |>
    dplyr::filter(!is.na(.data$sample_name)) |>
    ggplot2::ggplot(aes(x = .data$z_plot, y = .data$sample_name, color = .data$keep_logi, shape = .data$overflow)) +
    ggplot2::scale_color_manual(values = c("#666666", "#00AAAA")) +
    ggplot2::scale_shape_manual(values = c(16, NA)) +
    ggplot2::geom_text(aes(label = .data$label), size = 5) +
    ggplot2::geom_rect(xmin = -3, xmax = 3, ymin = 0, ymax = nrow(lib_qc$outliers), fill = "#AACCCC",  alpha = 0.1,  color = NA) +
    ggplot2::geom_vline(xintercept = 0, color = "#555555") +
    ggplot2::geom_point(size = 5) +
    ggplot2::xlab("Z-score") +
    ggplot2::coord_cartesian(xlim = c(-11.5, 11.5),
                             ylim = c(0.5, length(unique(lib_qc$outliers$sample_name)) + 0.5),
                             expand = FALSE) +
    ggplot2::theme(panel.grid.major.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   legend.position = "none")
}

#' Plot concentration of libraries across samples
#'
#' @param lib_qc Output of `pcr_lib_qc`
#'
#' @return a `ggplot`
#' @export
#'
#' @examples
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy(pad_zero = TRUE) |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc() |>
#'   pcr_lib_qc_plot_conc()
pcr_lib_qc_plot_conc <- function(lib_qc) {
  lib_qc$sample_summary |>
    ggplot2::ggplot(aes(x = .data$sample_name, y = .data$concentration_mean)) +
    ggplot2::geom_point(color = "#00AAAA", size = 5) +
    ggplot2::theme(axis.title = element_blank(),
                   legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
}

#' Plot the log of library quantities vs Ct
#'
#' @param lib_qc Output of `pcr_lib_qc`
#'
#' @return a `ggplot`
#' @export
#'
#' @details An optimal plot will have a slope of -3.32. This is because we
#'   expect that a sample 10x more concentrated than another will reach the same
#'   abundance in 3.32 doublings FASTER (that is, 3.32 fewer doubles, or Cts).
#'   Therefore, for each 10x increase in concentration (one point left to right
#'   on the plot) we expect a decrease in CT of 3.32. A steeper slope (more
#'   negative) implies a poorer efficiency (more cycles are required to reach
#'   10x than perfect doubling would imply)
#'
#' @examples
#' system.file("extdata", "untidy-standard-curve.xlsx", package = "amplify") |>
#'   pcr_tidy(pad_zero = TRUE) |>
#'   pcr_lib_calc() |>
#'   pcr_lib_qc() |>
#'   pcr_lib_qc_plot_slope()
pcr_lib_qc_plot_slope <- function(lib_qc) {
  slope_text <-
    dplyr::tibble(x = -2.5, y = 20,
                  label = paste(paste("Slope:", round(lib_qc$standards$slope[1], 2)),
                                paste("R\u00B2:", round(lib_qc$standards$r2[1], 2)),
                                paste0("Efficiency: ", round(lib_qc$standards$efficiency[1], 1), "%"),
                                sep = "\n"))

  lib_qc$standards |>
    ggplot2::ggplot(aes(x = log10(.data$quantity), y = .data$ct)) +
    ggplot2::geom_point(color = "#00AAAA", size = 5) +
    ggplot2::geom_smooth(method = "lm", se = F, color = "#666666", formula = "y ~ x") +
    ggplot2::geom_text(data = slope_text, aes(x = .data$x,  y = .data$y, label = .data$label), size = 8) +
    ggplot2::xlab("Log10(Quantity)") +
    ggplot2::ylab("CT")
}
