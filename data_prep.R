library("tidyverse")
library("janitor")
library("readxl")
library("fs")

files <- dir_ls("data", regexp = "\\.xlsx$")

# Remove submission time, yes/no questions, morning/afternoon/evening, CC staff type, LB names.
df <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(4, 9:10, 15, 20, 49:50, 72, 74, 76, 78, 80)) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

# online_session, offline_session, adults_offline, children_offline, total_offline, adults_online, children_online, total_online, session_datetime, session_location

source("delivery_agents.R")
