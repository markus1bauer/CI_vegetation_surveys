#' data_raw_sites
#'
#' A dataset containing the sites raw data
#'
#' @format A tibble with 288 rows and 17 variables:

sites <- read_csv(
  here("data", "data_raw_sites.csv"),
  lazy = TRUE,
  col_names = TRUE,
  na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "?",
      vegetation_cover.2018 = "d",
      vegetation_cover.2019 = "d",
      vegetation_cover.2020 = "d",
      vegetation_cover.2021 = "d"
    )
)
