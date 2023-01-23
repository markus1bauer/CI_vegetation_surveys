### Packages ###
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
suppressPackageStartupMessages(library(lubridate))
library(naniar)
library(forcats)

### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#______________________________________________________________________________
## 1 Sites ####################################################################


sites_experiment <- read_csv(here("data-raw", "data_raw_sites.csv"), col_names = TRUE,
                             na = c("", "NA", "na"),
                             col_types =
                               cols(
                                 .default = "?",
                                 survey_date.seeded = col_date(format = "%Y-%m-%d"),
                                 survey_date.2018 = col_date(format = "%Y-%m-%d"),
                                 survey_date.2019 = col_date(format = "%Y-%m-%d"),
                                 survey_date.2020 = col_date(format = "%Y-%m-%d"),
                                 survey_date.2021 = col_date(format = "%Y-%m-%d"),
                                 botanist.2018 = "c",
                                 botanist.2019 = "c",
                                 botanist.2020 = "c",
                                 botanist.2021 = "c",
                                 vegetation_cover.2018 = "d",
                                 vegetation_cover.2019 = "d",
                                 vegetation_cover.2020 = "d",
                                 vegetation_cover.2021 = "d",
                                 biomass.2019 = "d"
                               )) %>%
  pivot_longer(
    starts_with("vegetation_cover") |
      starts_with("botanist") |
      starts_with("biomass") |
      starts_with("survey_date"),
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
         vegetation_cover = as.numeric(vegetation_cover),
         biomass = as.numeric(biomass)) %>%
  filter(!(site == "C" & (survey_year == "seeded" |
                            survey_year == "2018" |
                            survey_year == "2019" |
                            survey_year == "2020" |
                            survey_year == "2021")))



#______________________________________________________________________________
## 2 Species ###################################################################


species_experiment <- data.table::fread(here("data-raw", "data_raw_species_20211112.csv"),
                                        sep = ",",
                                        dec = ".",
                                        skip = 0,
                                        header = TRUE,
                                        na.strings = c("", "NA", "na"),
                                        colClasses = list(
                                          character = "name"
                                        )) %>%
  ### Check that each species occurs at least one time ###
  group_by(name) %>%
  arrange(name) %>%
  select(name, all_of(sites_experiment$id)) %>%
  mutate(total = sum(c_across(
    starts_with("L") | starts_with("W")),
    na.rm = TRUE),
    presence = if_else(total > 0, 1, 0)) %>%
  # filter only species which occur at least one time:
  filter(presence == 1) %>%
  ungroup() %>%
  select(name, sort(tidyselect::peek_vars()), -total, -presence) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))



#______________________________________________________________________________
## 3 Traits ####################################################################


traits <- read_csv(here("data-raw", "data_raw_traits.csv"), col_names = TRUE,
                   na = c("", "NA", "na"),
                   col_types =
                     cols(
                       .default = "c",
                       name = "c",
                       l = "d",
                       t = "d",
                       k = "d",
                       f = "d",
                       r = "d",
                       n = "d",
                     )) %>%
  separate(name, c("genus", "species", "ssp", "subspecies"), "_",
           remove = FALSE, extra = "drop", fill = "right") %>%
  mutate(genus = str_sub(genus, 1, 4),
         species = str_sub(species, 1, 4),
         subspecies = str_sub(subspecies, 1, 4),
         name = as.character(name)) %>%
  unite(abb, genus, species, subspecies, sep = "", na.rm = TRUE) %>%
  mutate(abb = str_replace(abb, "NA", ""),
         abb = as_factor(abb)) %>%
  select(-ssp, -synonym, -nomenclature, -legal, -l, -k, -fchange) %>%
  arrange(name)

### Check congruency of traits and species table ###
anti_join(traits, species_experiment, by = "name") %>% select(name)
anti_join(species_experiment, traits, by = "name") %>% select(name)

### Combine with species_experiment table ###
traits <- traits %>%
  semi_join(species_experiment, by = "name")
