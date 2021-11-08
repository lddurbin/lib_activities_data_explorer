library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

raw_data <- readRDS(here::here("data/joined_data.rds"))

raw_data %>%
  select(id, delivery_datetime, delivery_library_names, unit_3_teams, location) %>% 
  mutate(outreach = case_when(
    delivery_library_names != location ~ TRUE,
    TRUE ~ FALSE
  ))
