# Generate reports looking at latest raw data ####
## this script is run automatically when there is a push


### Packages ###
library(dplyr)
library(forcats)
library(ggplot2)
library(gt)
library(here)
library(kableExtra)
library(knitr)
#library(magick)
library(naniar)
library(pak)
library(readr)
library(renv)
library(stringr)
library(tibble)
library(tidyr)
#library(webshot)
#library(webshot2)
#webshot::install_phantomjs(force = TRUE)


### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


data <- read_csv(
  here("data", "data_raw_sites.csv"),
  col_names = TRUE, na = c("", "NA", "na"),
  col_types = cols(.default = "?")
  ) %>%
  pivot_longer(
    starts_with("vegetation_cover"),
    names_to = c("x1", "survey_year"),
    names_sep = "\\.",
    values_to = "vegetation_cover",
    values_transform = list (vegetation_cover = as.numeric)
  ) %>%
  mutate(
    plot = str_replace(plot, "-", "_"),
    plot = str_replace(plot, "L_", "L"),
    plot = str_replace(plot, "W_", "W"),
    id = str_c(plot, survey_year, sep = "_")
  ) %>%
  select(id, survey_year, vegetation_cover)

sites <- read_csv(
  here("data", "data_raw_sites.csv"),
  col_names = TRUE, na = c("", "NA", "na"),
  col_types = cols(.default = "?")
) %>%
  pivot_longer(
    starts_with("survey_date"),
    names_to = c("x1", "survey_year"),
    names_sep = "\\.",
    values_to = "survey_date"
  ) %>%
  mutate(
    plot = str_replace(plot, "-", "_"),
    plot = str_replace(plot, "L_", "L"),
    plot = str_replace(plot, "W_", "W"),
    id = str_c(plot, survey_year, sep = "_")
  ) %>%
  select(id, survey_date) %>%
  full_join(data, by = "id") %>%
  filter(!str_detect(id, "seeded"))



## 2 Species ###################################################################


species <- data.table::fread(
  here("data", "data_raw_species.csv"),
  sep = ",", dec = ".", skip = 0, header = TRUE,
  na.strings = c("", "NA", "na"),
  colClasses = list(character = "name")
  ) %>%
  pivot_longer(-name, names_to = "id", values_to = "abundance") %>%
  filter(!str_detect(id, "seeded"))



## 3 Traits ####################################################################


traits <- read_csv(
  here("data", "data_raw_traits.csv"),
  lazy = TRUE, col_names = TRUE, na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "c",
      name = "c",
      f = "d",
      r = "d",
      n = "d"
      )
  )

### Combine with species table ###
traits <- traits %>%
  semi_join(species, by = "name")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Check data #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Typos vegetation cover ##################################################


### Set scale of total vegetation cover ###
values <- seq(from = 0, to = 100, by = 5)

### Check typos of sites cover ###
data <- sites %>%
  filter(!(vegetation_cover %in% values) & !is.na(vegetation_cover))

file <- here("tests", "testthat", "warnings_sites_typos.png")

if(count(data) == 0) {

  if(file.exists(file)) {

    file.remove(file)

  }

} else {

  data %>%
    gt() %>%
    tab_options(table.font.size = px(10)) #%>%
    #gtsave(file)

}


write_csv(
  data,
  here("tests", "testthat", "warnings_sites_typos.csv")
  )


## 2 Typos abundance values ###################################################


### Set scale of species vegetation cover ###
values <- c(.5, 2, 3, 4, seq(from = 0, to = 100, by = 5))

### Check typos of species ###
data <- species %>%
  filter(!(abundance %in% values) & !is.na(abundance))

file <- here("tests", "testthat", "warnings_species_typos.png")

if(count(data) == 0) {

  if(file.exists(file)) {

    file.remove(file)

  }

} else {

  data %>%
    gt() %>%
    tab_options(table.font.size = px(10)) #%>%
    #gtsave(file)

}

write_csv(
  data,
  here("tests", "testthat", "warnings_species_typos.csv")
  )


## 3 Compare vegetation_cover and accumulated_cover ###########################


data <- species %>%
  group_by(id) %>%
  summarise(total_cover = sum(abundance, na.rm = TRUE)) %>%
  full_join(sites, by = "id") %>%
  mutate(difference = (total_cover - vegetation_cover)) %>%
  select(id, survey_year, vegetation_cover, total_cover, difference) %>%
  filter(difference > 20 | difference < -5) %>%
  arrange(survey_year, id, difference)

file <- here("tests", "testthat", "warnings_different_total_cover.png")

if(count(data) == 0) {

  if(file.exists(file)) {

    file.remove(file)

  }

} else {

  data %>%
    kable() %>%
    kable_paper() #%>%
    #as_image(file = file)

}

readr::write_csv(
  data,
  here("tests", "testthat", "warnings_different_total_cover.csv")
)



## 4 Check missing data ########################################################


### a Sites -------------------------------------------------------------------

#### * Percentage ####

data <- miss_var_summary(sites, order = TRUE)

file <- here("tests", "testthat", "warnings_missing_plots.png")

if(count(data) == 0) {

  if(file.exists(file)) {

    file.remove(file)

  }

} else {

  data %>%
    kable() %>%
    kable_paper() #%>%
    #as_image(file = file)

}

readr::write_csv(
  data,
  here("tests", "testthat", "warnings_missing_plots.csv")
)

#### * Exact missing plots ####

data <- sites %>%
  filter(is.na(vegetation_cover))

data %>%
  kable() %>%
  kable_paper() #%>%
#as_image(file = here("tests", "testthat", "warnings_missing_vegetation_cover.png"))


readr::write_csv(
  data,
  here("tests", "testthat", "warnings_missing_vegetation_cover.csv")
)


### b Species ------------------------------------------------------------------

data <- species %>%
  group_by(id) %>%
  summarise(total_cover = sum(abundance, na.rm = TRUE)) %>%
  left_join(sites, by = "id") %>%
  select(id, survey_year, total_cover) %>%
  filter(total_cover < 40 & !str_detect(id, "_2018") & !str_detect(id, "_2019")) %>%
  arrange(survey_year, id)

data %>%
  kable() %>%
  kable_paper() #%>%
#as_image(file = here("tests", "testthat", "warnings_missing_abundances.png"))


readr::write_csv(
  data,
  here("tests", "testthat", "warnings_missing_abundances.csv")
)


### c Traits -------------------------------------------------------------------

miss_var_summary(traits, order = TRUE)

vis_miss(traits, cluster = FALSE, sort_miss = TRUE) +
  theme(plot.background = element_rect(fill = "white"))

ggsave(
  here("tests", "testthat", "reports_missing_traits.png"),
  dpi = 300, height = 10, units = "cm"
)
