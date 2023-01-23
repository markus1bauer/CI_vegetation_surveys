library(testthat)
library(dplyr)
library(readr)
context("Checks that all values in period variable are valid.")

base_data <- read_csv("../data-raw/data_raw_sites.csv")

valid_values <- c(0, 2, 4, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

test_that(
  desc = "Vegetation cover values are valid.",
  code = {
    all_vegetation_cover_valid <- all(
      sites_experiment$vegetation_cover %in% valid_values
      )
    testthat::expect_true(all_vegetation_cover_valid)
  }
)

#______________________________________________________________________________
## 4 Check data frames #########################################################


### Check typos ###
sites_experiment %>%
  filter(!str_detect(id, "_seeded$")) %>%
  janitor::tabyl(vegetation_cover)
#sites %>% filter(vegetation_cover == 17)
species_experiment %>%
  select(-name, -ends_with("_seeded")) %>%
  unlist() %>%
  janitor::tabyl()
species_experiment %>% # Check special typos
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  filter(value == 90)

### Compare vegetation_cover and accumulated_cover ###
species_experiment %>%
  summarise(across(where(is.double), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "id", values_to = "value") %>%
  mutate(id = factor(id)) %>%
  full_join(sites_experiment, by = "id") %>%
  mutate(diff = (value - vegetation_cover)) %>%
  select(id, survey_year, vegetation_cover, value, diff) %>%
  filter(!str_detect(id, "_seeded$")) %>%
  filter(diff > 20 | diff < -5) %>%
  arrange(survey_year, id, diff) %>%
  print(n = 100)

### Check plots over time ###
species_experiment %>%
  select(name, starts_with("L1_19"), -ends_with("_seeded")) %>%
  filter(if_any(starts_with("L"), ~ . > 0)) %>%
  print(n = 100)

### Check missing data ###
miss_var_summary(sites_experiment, order = TRUE)
vis_miss(sites_experiment, cluster = FALSE)
miss_var_summary(traits, order = TRUE)
vis_miss(traits, cluster = FALSE, sort_miss = TRUE)
