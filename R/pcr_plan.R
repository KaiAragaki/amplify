#' Plan PCR experiment
#'
#' @param data a data.frame, with samples as the first column (if `has_names = TRUE`) and RNA concentrations as the second (or first, if `has_names = FALSE`)
#' @param n_primers integer. Number of primers to be used in the experiment.
#' @param format integer. 96 or 384 - the number of wells of the plate planned to be used
#' @param exclude_border logical. Should the border be excluded to avoid edge effects? Default is TRUE.
#' @param primer_names character vector. Names of primers.
#' @param headless logical. If FALSE, return invisible and redirect to shiny application.
#' @param has_names logical. Is the first column the names of the samples?
#'
#' @importFrom rlang .data
#'
#' @return a named list
#' @export
#'
#' @examples
#' dummy_rna_conc |>
#'   pcr_plan(n_primers = 3)
pcr_plan <- function(data, n_primers, format = 384, exclude_border = TRUE,
                     primer_names = NULL, headless = TRUE, has_names = TRUE) {


  # Checks ---------------------------------------------------------------------
  if (!headless) {
    utils::browseURL("https://kai-a.shinyapps.io/plan-pcr/")
    return(invisible())
  }

  if (n_primers < length(primer_names)) {
    cli::cli_warn("More primer names ({length(primer_names)}) than n_primers ({n_primers}) given. Only using the first {n_primers}.")
    primer_names <- primer_names[1:n_primers]
  }

  format <- match.arg(as.character(format), c("96", "384"))

  data <- dplyr::as_tibble(data) # Allows for vector input

  if (ncol(data) == 1 & has_names) {
    stop("Data only has one column - did you mean `has_names = FALSE`?")
  }

  # Experimental constants -----------------------------------------------------
  ntc <- 1 # Non-targeting control - add one to sample number
  final_rna_conc <- 5#ng/uL Final [RNA], determined by protocol
  reps <- 3 # Perform in triplicate
  safety_reps <- 6 # Extra, since nothing is ever perfect
  rna_per_well <- 2#uL Vol RNA/well

  # Runtime constants ----------------------------------------------------------
  n_samples <- nrow(data)

  sample_names <- get_sample_names(data, has_names)

  if (!has_names)
    data <- cbind(sample_names, data)

  plate <- gp(wells = as.numeric(format))

  if (exclude_border)
    plate <- gp_sec(plate, "no_border", nrow = plate$nrow - 2, ncol = plate$ncol - 2, margin = 1)

  # Primer names ---------------------------------------------------------------
  pn <- paste("Primer", 1:n_primers)

  if (!missing(primer_names))
    pn[1:length(primer_names)] <- primer_names

  with_primers <- gp_sec(plate, "primers", nrow = n_samples + ntc, ncol = reps, break_sections = FALSE, labels = pn)

  max_sections <- max(with_primers$well_data$.sec, na.rm = TRUE)

  if (max_sections < n_primers) {
    with_primers <- gp_sec(plate, "primers", nrow = n_samples + ntc, ncol = reps, break_sections = FALSE, flow = "col", wrap = TRUE, labels = pn)
    max_sections_wrap <- max(with_primers$well_data$.sec, na.rm = TRUE)

    if (max_sections_wrap < n_primers)
      rlang::abort("This experiment requires too many wells.")
  }

  # FIXME Make sure this supports supplied sample names less than total number of samples
  # Will need to roll in sample name arg. Not too hard!
  with_samples <- gp_sec(with_primers, "samples", nrow = 1, wrap = TRUE, labels = sample_names)

  final_vol <- ((n_primers * reps) + safety_reps) * rna_per_well |> as.integer()

  # Sample preparation  --------------------------------------------------------
  sample_prep <- data |>
    dplyr::mutate(vol_to_add = final_rna_conc * final_vol / data[[2]]) |>
    dplyr::rowwise() |>
    dplyr::mutate(dilution_factor = get_best_factor(.data$vol_to_add)) |>
    dplyr::ungroup() |>
    dplyr::mutate(diluted_concentration = data[[2]] / .data$dilution_factor,
                  final_vol = final_vol,
                  diluted_rna_to_add = final_rna_conc * .data$final_vol / .data$diluted_concentration,
                  water_to_add = .data$final_vol - .data$diluted_rna_to_add) |>
    dplyr::select(-.data$vol_to_add) |>
    dplyr::relocate(.data$final_vol, .after = dplyr::last_col())

  # Mastermix Preparation ------------------------------------------------------
  mm <- dplyr::tibble(
    reagent = c("2X RT-PCR Buffer", "Primer", "25X RT-PCR Enzyme", "Nuclease Free H2O"),
    vol = c(6.25, .625, .5, 3.125) * (n_samples + ntc + 2) * reps
  )

  list(mm_prep = mm, sample_prep = sample_prep, plate = with_samples,
       n_primers = n_primers, format = format, exclude_border = exclude_border,
       primer_names = primer_names)
}

#' Create a report from a PCR plan
#'
#' @param pcr_plan output from `pcr_plan`
#' @param file_path  Where the report should be written, as well as the file name. Defaults to temp file.
#'
#' @return a named list, like the output of `pcr_plan`, but with the output file path appended.
#' @export
#'
#' @examples
#' dummy_rna_conc |>
#'   pcr_plan(n_primers = 3) |>
#'   pcr_plan_report()
pcr_plan_report <- function(pcr_plan, file_path = NULL) {

  if (missing(file_path)) {
    file_path <- tempfile(pattern = paste0(Sys.Date(), "_pcr-report_"),
                          fileext = ".html")
  }

  output_dir <- stringr::str_remove(file_path, "[^/]*$")
  output_file <- stringr::str_extract(file_path, "[^/]*$")
  rmarkdown::render(system.file("rmd", "pcr_report-template.Rmd", package = "amplify"),
                    output_dir = output_dir,
                    output_file = output_file,
                    params = list(sample_prep = pcr_plan$sample_prep,
                                  mm_prep = pcr_plan$mm_prep,
                                  plate = pcr_plan$plate,
                                  n_primers = pcr_plan$n_primers,
                                  primer_names = pcr_plan$primer_names,
                                  format = pcr_plan$format,
                                  exclude_border = pcr_plan$exclude_border))

  c(pcr_plan, list(file_path = file_path))
}

#' Get or make sample names
#'
#' If the user did not supply sample names (`has_names = FALSE`), sample names
#' will be generated in the form of "Sample_n".
#'
#' @param data A data.frame
#' @param has_names logical. Should we expect sample names to be in the first
#'   column?
#'
#' @return A vector of sample names
#'
#' @keywords internal
get_sample_names <- function(data, has_names) {
  names <- ifelse(has_names, data[[1]], paste("Sample", 1:nrow(data))) |>
    c("NTC")
}

#' Calculate a sensible dilution factor
#'
#' If the volume of RNA to add is < 1uL, it must be diluted. Dilutions are easy
#' to calculate in one's head only if they are integers (divisible by 5
#' preferred). Further, this dilution should be as small as reasonably possible,
#' otherwise it will become too dilute.
#'
#' @param vol_to_add numeric. 'Naive' volume to add, before dilution.
#'
#' @return an integer, either 1 (no dilution) or something divisible by 5.
#'
#' @keywords internal
get_best_factor <- function(vol_to_add) {
  dplyr::if_else(vol_to_add < 1, {
    as.integer(ceiling((1 / vol_to_add)/5) * 5) # Give something divisible by 5
  }, 1L)
}
