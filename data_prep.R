library("tidyverse")
library("janitor")
library("readxl")
library("fs")
library("lubridate")

source("functions.R")

files <- dir_ls("data", regexp = "\\.xlsx$")

# Remove submission time, yes/no questions, morning/afternoon/evening, CC staff type.
df_1 <- files %>% 
  map_dfr(read_excel) %>% 
  select(-c(4, 9:10, 15, 49, 72, 74, 76, 78, 80)) %>% 
  clean_names() %>% 
  mutate(id = as.character(id))

# Online/offline columns, concatenate locations into 1 column, datetime column
df_2 <- df_1 %>% 
  select(-c(41:43, 45:66)) %>%
  mutate(
    in_person = str_detect(how_was_the_session_delivered, "person"),
    online = str_detect(how_was_the_session_delivered, "Online"),
    .keep = "unused"
    ) %>% 
  unite("location", starts_with("where_in"), na.rm = TRUE, remove = TRUE) %>% 
  unite("sublocation", c(17:19), na.rm = TRUE, remove = TRUE) %>% 
  unite("delivery_time", starts_with("at_what_time"), na.rm = TRUE, remove = TRUE) %>% 
  mutate(delivery_datetime = parse_date_time(paste(when_was_the_session_delivered, delivery_time), c("Ymd HMp", "Ymd Hp")), .keep = "unused")

age_groups <- multichoice_splitting(df_1, what_was_the_target_age_group_for_this_session, age_group)

target_groups <- multichoice_splitting(df_1, which_of_these_groups_was_the_session_designed_to_benefit, target_group)

realm_languages <- multichoice_splitting(df_1, which_language_s_was_the_session_delivered_in, realm_language)

source("delivery_agents.R")

# df_x <- df_y %>% 
#   left_join(delivery_agents, by = "id")
