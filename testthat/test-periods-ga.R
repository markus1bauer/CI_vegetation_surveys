library(testthat)
library(dplyr)
library(readr)
context("Checks that all values in period variable are valid.")

base_data <- read_csv("../data-raw/data_raw_sites.csv")

test_that(
  desc = "Vegetation cover values are valid.",
  code = {
    all_vegetation_cover_valid <- all(sites_experiment$vegetation_cover < 100)
    expect_true(all_vegetation_cover_valid)
  }
)
