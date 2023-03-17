library(here)


base_data <- readr::read_csv(
  here("data", "data_raw_sites.csv"),
  col_types = "?"
  ) %>%
  as.data.frame()

testthat::test_that(
  desc = "Values are valid",
  code = {
    all_values_valid <- all(base_data$vegetaion_cover.2018 < 101)
    testthat::expect_true(all_values_valid)
  }
)


#context("checks that CI_1_Generate_reports.R is working")

#test_that("printed value is correct", {

  #expect_output(
#    str(source(here("R", "CI_1_Generate_reports.R"))),
#    "Have this script run whatever data cleaning you do"
#    )
#  })

#test_that("There are different total cover values", {

#  report_exists  <-  ifelse(
#    file.exists(
#      file.path(
#        here("outputs", "different_total_cover.csv")
#        )
#      ),
#    TRUE, FALSE
#    )

  #expect_false(report_exists)

#})
