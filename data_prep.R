library("tidyverse")
library("janitor")
library("readxl")
library("fs")
library("lubridate")

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
  select(-c(41:43, 45:66)) %>%
  mutate(
    in_person = str_detect(how_was_the_session_delivered, "person"),
    online = str_detect(how_was_the_session_delivered, "Online")
    ) %>% 
  unite("location", starts_with("where_in"), na.rm = TRUE, remove = TRUE) %>% 
  unite("sublocation", c(18:20), na.rm = TRUE, remove = TRUE) %>% 
  unite("delivery_time", starts_with("at_what_time"), na.rm = TRUE) %>% 
  select(-c("how_was_the_session_delivered", starts_with("at_what_time")))

df_3 <- df_2 %>% 
  left_join(delivery_agents, by = "id")

df_4 <- df_3 %>% 
  mutate(delivery_datetime = parse_date_time(paste(when_was_the_session_delivered, delivery_time), c("Ymd HMp", "Ymd Hp")))
