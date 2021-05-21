library("tidyverse")
library("janitor")
library("readxl")
library("fs")

files <- dir_ls("data", regexp = "\\.xlsx$")

# Remove submission time, yes/no questions, morning/afternoon/evening, CC staff type.
df_1 <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(4, 9:10, 15, 49, 72, 74, 76, 78, 80)) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

source("delivery_agents.R")

# Online/offline columns, concatenate locations into 1 column
df_2 <- df_1 %>% 
  select(-c(40:42, 44:64)) %>%
  mutate(in_person = case_when(
    str_detect(how_was_the_session_delivered, "person") == TRUE ~ TRUE,
    str_detect(how_was_the_session_delivered, "person") == FALSE ~ FALSE
  ),
  online = case_when(
    str_detect(how_was_the_session_delivered, "Online") == TRUE ~ TRUE,
    str_detect(how_was_the_session_delivered, "Online") == FALSE ~ FALSE
  )) %>% 
  unite("location", starts_with("where_in"), na.rm = TRUE, remove = TRUE) %>% 
  unite("sublocation", starts_with("in_which_"), na.rm = TRUE, remove = TRUE) %>% 
  select(-c("how_was_the_session_delivered"))


df_3 <- df_2 %>% 
  left_join(delivery_agents, by = "id")
