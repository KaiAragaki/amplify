
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amplify <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->
<!-- badges: end -->

**amplify** automates routine pcr-based tasks - including plate
planning, dilution making, visualizing, and analyzing - so rather than
thinking about your experiments themselves, you can think about what
your experiments *mean*.

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/amplify")
```

## Tidying qPCR data

Data from the QuantStudio software is exported in a fairly non-standard
format:

``` r
library(amplify)
library(readxl)

untidy_file_path <- system.file("extdata", "untidy-pcr-example.xls", package = "amplify")

untidy_file_path |> 
  readxl::read_excel() |> 
  head() |> 
  knitr::kable()
#> New names:
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * `` -> ...6
#> * `` -> ...7
#> * ...
```

| Block Type                                     | 384-Well Block | …3  | …4  | …5  | …6  | …7  | …8  | …9  | …10 | …11 | …12 | …13 |
|:-----------------------------------------------|:---------------|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|
| Calibration Background is expired              | Yes            | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |
| Calibration Background performed on            | 01-13-2020     | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |
| Calibration Normalization FAM-ROX is expired   | Yes            | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |
| Calibration Normalization FAM-ROX performed on | 01-13-2020     | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |
| Calibration Normalization VIC-ROX is expired   | Yes            | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |
| Calibration Normalization VIC-ROX performed on | 01-13-2020     | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  | NA  |

amplify provides `pcr_tidy` to automatically tidy these files:

``` r
untidy_file_path |> 
  pcr_tidy() |> 
  head() |> 
  knitr::kable()
#> New names:
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * `` -> ...6
#> * `` -> ...7
#> * ...
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

| well | well_position | omit  | sample_name | target_name | task    | reporter | quencher | quantity | quantity_mean | quantity_sd |  rq |    rq_min |   rq_max |       ct |  ct_mean |     ct_sd | delta_ct | delta_ct_mean      | delta_ct_se          | delta_delta_ct | automatic_ct_threshold | ct_threshold | automatic_baseline | baseline_start | baseline_end | comments | well_row | well_col | analysis_type | control | conf_int | ref_samp | plate_type     | exp_type |
|:-----|:--------------|:------|:------------|:------------|:--------|:---------|:---------|---------:|--------------:|------------:|----:|----------:|---------:|---------:|---------:|----------:|:---------|:-------------------|:---------------------|:---------------|:-----------------------|-------------:|:-------------------|---------------:|-------------:|:---------|---------:|---------:|:--------------|:--------|:---------|:---------|:---------------|:---------|
| 26   | B2            | FALSE | RD1         | KRT14       | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9316003 | 1.073422 | 29.70135 | 29.74859 | 0.0474225 | NA       | 7.8036408424377441 | 0.036815796047449112 | 0              | TRUE                   |    0.2075911 | TRUE               |              3 |           24 | NA       |        2 |        2 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
| 27   | B3            | FALSE | RD1         | KRT14       | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9316003 | 1.073422 | 29.79619 | 29.74859 | 0.0474225 | NA       | 7.8036408424377441 | 0.036815796047449112 | 0              | TRUE                   |    0.2075911 | TRUE               |              3 |           23 | NA       |        2 |        3 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
| 28   | B4            | FALSE | RD1         | KRT14       | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9316003 | 1.073422 | 29.74823 | 29.74859 | 0.0474225 | NA       | 7.8036408424377441 | 0.036815796047449112 | 0              | TRUE                   |    0.2075911 | TRUE               |              3 |           24 | NA       |        2 |        4 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
| 29   | B5            | FALSE | RD1         | CDH3        | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9305190 | 1.074669 | 28.92050 | 28.89161 | 0.0488189 | NA       | 6.946662425994873  | 0.037419259548187256 | 0              | TRUE                   |    0.5056610 | TRUE               |              3 |           23 | NA       |        2 |        5 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
| 30   | B6            | FALSE | RD1         | CDH3        | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9305190 | 1.074669 | 28.91909 | 28.89161 | 0.0488189 | NA       | 6.946662425994873  | 0.037419259548187256 | 0              | TRUE                   |    0.5056610 | TRUE               |              3 |           22 | NA       |        2 |        6 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
| 31   | B7            | FALSE | RD1         | CDH3        | UNKNOWN | FAM      | NFQ-MGB  |       NA |            NA |          NA |   1 | 0.9305190 | 1.074669 | 28.83525 | 28.89161 | 0.0488189 | NA       | 6.946662425994873  | 0.037419259548187256 | 0              | TRUE                   |    0.5056610 | TRUE               |              3 |           22 | NA       |        2 |        7 | Singleplex    | GAPDH   | 95.0     | RD1      | 384-Well Block | comp     |
