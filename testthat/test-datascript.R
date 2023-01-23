library(testthat)
context("checks that CI_1_Generate_reports is working")

test_that("printed value is correct", {
  
  expect_output(str(source("../CI_1_Generate_reports.R")), 
                   "Have this script run whatever data cleaning you do")

})
