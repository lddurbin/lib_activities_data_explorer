library("tidyverse")
library("janitor")
library("readxl")
library("fs")

process_multichoice <- function(df, selection) {
  df %>% 
    mutate(across(selection, ~str_sub(.x, 2, -2))) %>% 
    separate_rows(selection, sep = ",") %>% 
    mutate(across(selection, ~str_sub(.x, 2, -2)))
}

files <- dir_ls("data", regexp = "\\.xlsx$")

# Remove submission time, yes/no questions, morning/afternoon/evening, CC staff type.
df <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(4, 9:10, 15, 49, 72, 74, 76, 78, 80)) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

# online_session, offline_session, adults_offline, children_offline, total_offline, adults_online, children_online, total_online, session_datetime, session_location

delivery_agents <- df %>% 
  select(1, 41:43, 45:66) %>%
  select(id, agent_types = 2, external_agent_names = 3, AC_agent_names = 4, local_board_names = 5, unit_names = 22, unit_1_teams = 23, unit_2_teams = 24, unit_3_teams = 25, unit_4_teams = 26, everything()) %>% 
  pivot_longer(11:length(.), values_to = "library_names", values_drop_na = TRUE) %>% 
  mutate(across(c("agent_types", "local_board_names", "library_names"), ~str_sub(.x, 2, -2))) %>% 
  separate_rows(c("agent_types", "local_board_names", "library_names"), sep = ",") %>% 
  mutate(across(c("agent_types", "local_board_names", "library_names"), ~str_sub(.x, 2, -2)))
  