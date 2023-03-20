# Generate warnings looking at the reports that are not looked at with testhat ####
## this script is run automatically when there is a push



### Packages ###
library(dplyr)
library(forcats)
library(ggplot2)
library(here)
library(naniar)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

### Start ###
rm(list = ls())


#### Load files ####

path <- file.path(here("testthat", "warnings_file.csv"))
if(file.exists(path)) warning_file <- read.csv(path)


# write warning messages ####

warning_messages <- c(
  "unhealthy_but_wrong_status" = "There are living trees that are unhealthy but status is not AU.",
  "wounded_level_but_wrong_status_or_FAD" = "There are trees with wounded level but no W in FAD."
  )


# check if files exist and generate a plot with the warning ####


if(exists("warning_file"))
  all_warns <- paste(
    c(
      "WARNINGS!!!\n",
      warning_messages[unique(warning_file$warning_name)],
      "CLICK HERE TO GO TO FOLDER"
      ),
    collapse = "\n"
    ) else all_warns = "No WARNINGS"
