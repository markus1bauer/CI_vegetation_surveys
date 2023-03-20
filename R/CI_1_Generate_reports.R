# Generate reports looking at latest raw data ####
## this script is run automatically when there is a push


### Packages ###
library(dplyr)
library(forcats)
library(ggplot2)
library(gt)
library(here)
library(naniar)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


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
  ) %>%
  pivot_longer(
    starts_with("vegetation_cover"),
    names_to = c("x", "survey_year"),
    names_sep = "\\.",
    values_to = "n",
    values_transform = list (n = as.character)
  ) %>%
  pivot_wider(names_from = "x", values_from = "n") %>%
  mutate(plot = str_replace(plot, "-", "_"),
         plot = str_replace(plot, "L_", "L"),
         plot = str_replace(plot, "W_", "W"),
         id = str_c(plot, survey_year, sep = "_"),
         plot = factor(plot),
         id = factor(id),
         vegetation_cover = as.numeric(vegetation_cover)) %>%
  filter(!(site == "C" & (survey_year == "seeded" |
                            survey_year == "2018" |
                            survey_year == "2019" |
                            survey_year == "2020" |
                            survey_year == "2021")))



## 2 Species ###################################################################


species <- data.table::fread(
  here("data", "data_raw_species.csv"),
  sep = ",",
  dec = ".",
  skip = 0,
  header = TRUE,
  na.strings = c("", "NA", "na"),
  colClasses = list(
    character = "name"
    )
  ) %>%
  ### Check that each species occurs at least one time ###
  group_by(name) %>%
  arrange(name) %>%
  select(name, tidyselect::all_of(sites$id)) %>%
  mutate(total = sum(c_across(
    starts_with("L") | starts_with("W")),
    na.rm = TRUE),
    presence = if_else(total > 0, 1, 0)) %>%
  # filter only species which occur at least one time:
  filter(presence == 1) %>%
  ungroup() %>%
  select(name, sort(tidyselect::peek_vars()), -total, -presence) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))



## 3 Traits ####################################################################


traits <- read_csv(
  here("data", "data_raw_traits.csv"),
  lazy = TRUE,
  col_names = TRUE,
  na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "c",
      name = "c",
      f = "d",
      r = "d",
      n = "d"
      )
  ) %>%
  separate(name, c("genus", "species", "ssp", "subspecies"), "_",
           remove = FALSE, extra = "drop", fill = "right") %>%
  mutate(genus = str_sub(genus, 1, 4),
         species = str_sub(species, 1, 4),
         subspecies = str_sub(subspecies, 1, 4),
         name = as.character(name)) %>%
  unite(abb, genus, species, subspecies, sep = "", na.rm = TRUE) %>%
  mutate(abb = str_replace(abb, "NA", ""),
         abb = as_factor(abb)) %>%
  select(-ssp) %>%
  arrange(name)

### Check congruency of traits and species table ###
anti_join(traits, species, by = "name") %>% select(name)
anti_join(species, traits, by = "name") %>% select(name)

### Combine with species table ###
traits <- traits %>%
  semi_join(species, by = "name")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Check data #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Sites' vegetation cover ##################################################


### Set scale of total vegetation cover ###
values <- seq(from = 0, to = 100, by = 5)

### Check typos of sites cover ###
data <- sites %>%
  filter(!str_detect(id, "_seeded$")) %>%
  filter(!(vegetation_cover %in% values) &
           !is.na(vegetation_cover))

write_csv(
  data,
  here("tests", "testthat", "warnings_sites_typos.csv")
  )


## 2 Species' vegetation cover #################################################


### Set scale of species vegetation cover ###
values <- c(.5, 2, 3, 4, seq(from = 0, to = 100, by = 5))

### Check typos of species ###
data <- species %>%
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  filter(!str_detect(id, "_seeded$")) %>%
  filter(!(value %in% values) &
           !is.na(value))

write_csv(
  data,
  here("tests", "testthat", "warnings_species_typos.csv")
  )


## 3 Compare vegetation_cover and accumulated_cover ###########################


data <- species %>%
  summarise(across(where(is.double), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "id", values_to = "value") %>%
  mutate(id = factor(id)) %>%
  full_join(sites, by = "id") %>%
  mutate(diff = (value - vegetation_cover)) %>%
  select(id, survey_year, vegetation_cover, value, diff) %>%
  filter(!str_detect(id, "_seeded$")) %>%
  filter(diff > 20 | diff < -5) %>%
  arrange(survey_year, id, diff)


#data %>%
#  gt() %>%
#  gtsave(here("tests", "testthat", "warnings_different_total_cover.png"))

readr::write_csv(
  data,
  here("tests", "testthat", "warnings_different_total_cover.csv")
)



## 4 Check plots over time #####################################################


species %>%
  select(name, starts_with("L1_19"), -ends_with("_seeded")) %>%
  filter(if_any(starts_with("L"), ~ . > 0)) %>%
  print(n = 100)



## 5 Check missing data ########################################################


miss_var_summary(sites, order = TRUE)
vis_miss(sites, cluster = FALSE)
ggsave(
  here("tests", "testthat", "reports_missing_sites.png"),
  dpi = 300, width = 15, height = 9, units = "cm"
)
miss_var_summary(traits, order = TRUE)
vis_miss(traits, cluster = FALSE, sort_miss = TRUE)
ggsave(
  here("tests", "testthat", "reports_missing_traits.png"),
  dpi = 300, width = 15, height = 9, units = "cm"
)
