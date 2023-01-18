print("Have this script run whatever data cleaning you do")

library(here)
library(dplyr)
library(readr)

data <- read_csv(here("data-raw", "data.csv")) %>%
  select(period, BA)
