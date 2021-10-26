#' Tidy a PCR Excel File
#'
#' Takes in a fresh results output from qPCR and converts it into a tidy
#' format. Useful for downstream analyses.
#'
#' @param file_path Path to an excel file containing the results of a qPCR
#'   run.
#' @param pad_zero logical. Should 'Sample 1' become 'Sample 01'?
#'
#' @return A `tibble`
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' dat_path <- system.file("extdata", "untidy-pcr-example.xls", package = "amplify")
#'
#' # Before tidying
#' dat_dirty <- readxl::read_excel(dat_path, sheet = "Results")
#' dat_dirty[1:10, 1:5]
#'
#' # After tidying
#' dat_clean <- pcr_tidy(dat_path)
#' dat_clean[1:10, 1:5]

pcr_tidy <- function(file_path, pad_zero = FALSE) {

  dat_og <- readxl::read_excel(file_path, sheet = "Results", .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

  exp_type <- substr(dat_og[which(dat_og$`Block Type`=="Experiment Type"),2], 0, 4)

  ind_start <- which(dat_og[,1] == "Well")

  if (exp_type == "Stan") {
    ind_end <- nrow(dat_og)
    dat <- dat_og[ind_start:ind_end,]
  }
  if (exp_type == "Comp") {
    ind_end <- which(dat_og[,1] == "Analysis Type")
    exp_dat <- dat_og[ind_end:nrow(dat_og), 1:2] |>
      t()
    colnames(exp_dat) <- make.names(exp_dat[1,])
    exp_dat <- exp_dat[-1,]
    exp_dat <- t(exp_dat) |>
      as.data.frame()
    dat <- dat_og[-c(1:(ind_start-1), (ind_end-1):nrow(dat_og)),]
  }

  names <- dat[1,] |>
    stringr::str_replace_all("[[:space:]-]", "_") |>
    stringr::str_replace_all("\\(superscript_2\\)", "2") |>
    tolower()

  dat <- dat |>
    stats::setNames(names) |>
    dplyr::slice(-1) |>
    dplyr::mutate(dplyr::across(dplyr::matches("^(delta_)*ct.*|^rq|quantity|^baseline|y_intercept|r2|slope|efficiency"), \(x) suppressWarnings(as.numeric(x))),
                  dplyr::across(dplyr::matches("^automatic|omit"), as.logical),
                  well_row = stringr::str_extract(.data$well_position, "^.{1}"),
                  well_col = as.numeric(stringr::str_extract(.data$well_position, "[:digit:]{1,2}$")),
                  well_row = as.numeric(factor(.data$well_row, levels = LETTERS)))

  if (exp_type == "Comp") {
    dat$analysis_type <- exp_dat$Analysis.Type
    dat$control <- exp_dat$Endogenous.Control
    dat$conf_int <- exp_dat$RQ.Min.Max.Confidence.Level
    dat$ref_samp <- exp_dat$Reference.Sample
  }

  if (exp_type == "Stan") {
    dat <- dat |>
      mutate(sample_name = dplyr::case_when(is.na(.data$sample_name) & .data$quantity > 6.0000 ~ "1",
                                            is.na(.data$sample_name) & .data$quantity > 0.6000 ~ "1:10",
                                            is.na(.data$sample_name) & .data$quantity > 0.0600 ~ "1:100",
                                            is.na(.data$sample_name) & .data$quantity > 0.0060 ~ "1:1000",
                                            is.na(.data$sample_name) & .data$quantity > 0.0006 ~ "1:10000",
                                            TRUE ~ .data$sample_name))
  }


  if (pad_zero) {
    dat$sample_name <- pad_zero(dat$sample_name)
  }

  dat$plate_type <- colnames(dat_og)[2]
  dat$exp_type <- tolower(exp_type[[1]])

  dat
}
