library("tidyverse")
library("janitor")
library("readxl")
library("fs")

files <- dir_ls("data", regexp = "\\.xlsx$")

df <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(3:5, 10:11, 16, 21, 50, 73, 75, 77, 79, 81)) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

# online_session, offline_session, adults_offline, children_offline, total_offline, adults_online, children_online, total_online, session_datetime, session_location

delivery_agents <- df %>% 
  select(1, 40:41, 43:64) %>%
  select(id, CC_agent_names = 2, AC_agent_names = 3, local_board_names = 4, unit_names = 21, unit_1_teams = 22, unit_2_teams = 23, unit_3_teams = 24, unit_4_teams = 25, everything()) %>% 
  pivot_longer(10:length(.)-1, values_to = "library_name", values_drop_na = TRUE) %>% 
  select(-name) %>% 
  mutate(across(c(2:8, 10), ~str_sub(.x, end = -2))) %>% 
  separate_rows(2:9, sep = ";")
  