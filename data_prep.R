library("tidyverse") # for everything
library("janitor") # tidy the column headings
library("readxl") # source data is an Excel spreadsheet
library("fs") # to list files in a directory
library("lubridate") # easy date manipulation

source("functions.R")

files <- dir_ls("data", regexp = "\\.xlsx$")

# Remove yes/no questions, morning/afternoon/evening, CC staff type.
df <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(starts_with(c("Do you know", "Can you", "Did the session", "How would you describe", "Was the session")))) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

# Online/offline columns, concatenate locations into 1 column, datetime column
base_table <- df %>% 
  select(-c(42:44, 46:70)) %>%
  mutate(in_person = str_detect(how_was_the_session_delivered, "person"), online = str_detect(how_was_the_session_delivered, "Online"), .keep = "unused") %>% 
  unite("location", starts_with("where_in"), na.rm = TRUE, remove = TRUE) %>% 
  unite("sublocation", c(18:20), na.rm = TRUE, remove = TRUE) %>% 
  unite("delivery_time", starts_with("at_what_time"), na.rm = TRUE, remove = TRUE) %>% 
  mutate(delivery_datetime = parse_date_time(paste(when_was_the_session_delivered, delivery_time), c("Ymd HMp", "Ymd Hp")), .keep = "unused") %>% 
  mutate(across(c(3, 12:14, 16, 21), as.factor))

# one row per selection per session: age groups
age_groups <- multichoice_splitting(df, what_was_the_target_age_group_for_this_session, age_group)
target_groups <- multichoice_splitting(df, which_of_these_groups_was_the_session_designed_to_benefit, target_group)
realm_languages <- multichoice_splitting(df, which_language_s_was_the_session_delivered_in, realm_language)

source("delivery_agents.R")
