#' Tidy a PCR Excel File
#'
#' Takes in a fresh results output from qPCR and converts it into a tidy format.
#' Useful for downstream analyses.
#'
#' @param file_path Path to an excel file containing the results of a qPCR run.
#' @param usr_standards optional numeric vector specifying standard concentrations.
#'   If not supplied, if they exist they will be guessed. See details.
#' @param pad_zero logical. Should 'Sample 1' become 'Sample 01'?
#'
#' @details If a standards argument is not supplied AND if the experiment uses
#'   standards (ie is not a comparative Ct experiment), the standards will be
#'   guessed from the unique quantities with a 'STANDARD' task.
#'
#'   If standards ARE supplied, they will be matched to their nearest quantity,
#'   rounding UP. That is, if 1, .1, .01... and the actual quantities are 6.8,
#'   .68, .068..., 1 will be rounded to 6.8, .1 will be rounded to .68, etc
#'   (despite being numerically closer were they rounded down).
#'
#'   If standards are missing, (ie, only 1 and .01 are supplied), they will be
#'   dropped with a message. This is useful if you want to exclude an outlier
#'   sample from downstream analysis.
#'
#'
#'
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

pcr_tidy <- function(file_path, pad_zero = FALSE, usr_standards = NULL) {

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

    standards <- dat |>
      dplyr::select(quantity, task) |>
      dplyr::filter(task == "STANDARD") |>
      dplyr::arrange(quantity) |> # for findInterval
      dplyr::pull(quantity) |>
      unique()

    if (is.null(usr_standards)) {
      usr_standards <- 10^(0:-4) * 6
    }

    usr_standards <- usr_standards |>
      tibble::enframe(name = "name", value = "usr_quantity") |>
      dplyr::mutate(name = paste("Standard", 1:length(usr_standards))) |>
      dplyr::arrange(usr_quantity)

    if (nrow(usr_standards) > length(standards)) {
      stop("More standards supplied than exist in dataset")
    }

    if (max(usr_standards$usr_quantity) >= max(standards)) {
      stop("Largest standard supplied is greater than largest standard in dataset. \nSupplied standards are rounded up.")
    }

    fi_standards <- standards * 1.000001 # in case largest usr_stan = stan, this lets it get in between the interval

    intervals <- findInterval(usr_standards$usr_quantity, fi_standards)
    positions <- intervals + 1

    final <-
      tibble::tibble(positions,
             quantity = standards[positions],
             task = "STANDARD") |>
      dplyr::bind_cols(usr_standards)

    dat <- dplyr::left_join(dat, final, by = c("task", "quantity")) |>
      dplyr::mutate(sample_name = ifelse(is.na(sample_name), name, sample_name))

    dropping <- dat |>
      dplyr::filter(is.na(sample_name) & task == "STANDARD")

    if (nrow(dropping) > 0) {
      dat <- dat |>
        dplyr::filter(!is.na(sample_name) | task != "STANDARD")
      message(nrow(dropping), " rows of standards did not have a matching value in 'standards' and have been dropped")
      dat$slope <- lm(ct~log10(quantity), data = dat)$coefficients[2]
    }

  }


  if (pad_zero) {
    dat$sample_name <- pad_zero(dat$sample_name)
  }

  dat$plate_type <- colnames(dat_og)[2]
  dat$exp_type <- tolower(exp_type[[1]])

  dat
}
