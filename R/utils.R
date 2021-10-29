#' Add leading 0 to sample names
#'
#' @param sample_names a vector of sample names
#' @return A sample name with up to one zero padded
#' @export
#' @examples
#' c("blueberry", "Sample 1", "Sample 10", "sample 2", "john", "larry", "toxic waste", "Sample 02") |>
#'   pad_zero()
#'
#' # pad_zero will not pad if it doesn't need to:
#' c("Sample 1", "Sample 2", "Sample 9") |> pad_zero()
#'
#' # pad_zero is case sensitive, since these will be coming off a machine in a standardized format
#'
#' c("sample 10", "sample 1") |> pad_zero()
#' c("Sample 10", "Sample 1") |> pad_zero()

pad_zero <- function(sample_names) {

  nums <- stringr::str_extract(sample_names, "(?<=^(Sample|Standard) )[:digit:]{1,2}$")

  unique_nums <- unique(nums)

  if (all(is.na(nums))) return(sample_names)

  width_to_pad <- max(nchar(unique_nums), na.rm = TRUE)

  padded_nums <- stringr::str_pad(nums, width = width_to_pad, pad = "0")

  name_frame <-
    dplyr::tibble(sample_names, nums, padded_nums) |>
    dplyr::mutate(new_names = purrr::pmap_chr(list(sample_names, nums, padded_nums), stringr::str_replace),
                  new_names = ifelse(is.na(new_names), sample_names, new_names))

  name_frame$new_names
}

#' Recalcuate standard slope of quantity vs Ct
#'
#' @param tidy_pcr a object that has been tidied by `tidy_pcr`
#'
#' @return a tibble with an updated `slope` column
#' @export
pcr_calc_slope <- function(tidy_pcr) {
  tidy_pcr$slope <- stats::lm(ct~log10(quantity), data = tidy_pcr)$coefficients[2] |> unname()
  tidy_pcr
}
