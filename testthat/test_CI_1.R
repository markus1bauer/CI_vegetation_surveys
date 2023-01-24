library(testthat)
library(here)

context("checks that CI_1_Generate_reports is working")

test_that("printed value is correct", {

  expect_output(
    str(source(here("R", "CI_1_Generate_reports.R"))),
    "Have this script run whatever data cleaning you do"
    )
  })

test_that("There are different total cover values", {

  report_exists  <-  ifelse(
    file.exists(
      file.path(
        here("outputs", "different_total_cover.csv")
        )
      ),
    TRUE, FALSE
    )

  expect_false(report_exists)

})
