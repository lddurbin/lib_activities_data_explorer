library("tidyverse", warn.conflicts = FALSE) # for everything
library("janitor", warn.conflicts = FALSE) # tidy the column headings
library("readxl", warn.conflicts = FALSE) # source data is an Excel spreadsheet
library("fs", warn.conflicts = FALSE) # to list files in a directory
library("lubridate", warn.conflicts = FALSE) # easy date manipulation

source("functions.R")

files <- dir_ls("data", regexp = "\\.xlsx$")

normalised_locations <- read_csv("data/expected_locations.csv", col_types = "cc") %>%
  as_tibble() %>% 
  select(location)

# Remove yes/no questions, morning/afternoon/evening, CC staff type, test data entered by Lee
df <- files %>% 
  map_dfr(read_excel, guess_max = 1048576) %>% 
  select(-c(starts_with(c("Do you know", "Can you", "Did the session", "How would you describe", "Was the session")))) %>% 
  clean_names() %>% 
  mutate(id = as.character(id)) %>% 
  filter(email != "durbinl@aklc.govt.nz")

# Online/offline columns, concatenate locations into 1 column, datetime column
base_table_for_checker <- df %>% 
  select(-c(42:44, 46:70, 72:77)) %>%
  mutate(in_person = str_detect(how_was_the_session_delivered, "person"), online = str_detect(how_was_the_session_delivered, "Online"), .keep = "unused") %>% 
  unite("location", starts_with("where_in"), na.rm = TRUE, remove = TRUE) %>% 
  unite("sublocation", c(18:20), na.rm = TRUE, remove = TRUE) %>% 
  unite("delivery_time", starts_with("at_what_time"), na.rm = TRUE, remove = TRUE) %>% 
  mutate(
    delivery_datetime = parse_date_time(paste(when_was_the_session_delivered, delivery_time), c("Ymd HMp", "Ymd Hp")),
    what_was_the_duration_of_the_session_to_the_nearest_half_an_hour = time_length(str_remove(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, "and "), "minute"),
    what_was_the_format_of_the_session = case_when(
      !what_was_the_format_of_the_session %in% unlist(c("Book a Librarian", "Class or workshop", "Community event", "Club", "Live performance", "Pre-school activity", "Promotion or roadshow", "Talk")) ~ "Other",
      !is.na(what_was_the_format_of_the_session) ~ what_was_the_format_of_the_session
    ),
    .keep = "unused"
    )

base_table <- base_table_for_checker %>% 
  rowwise() %>% 
  mutate(location = case_when(
    str_length(sublocation) < 2 ~ location,
    str_length(sublocation) > 1 ~ sublocation
  ),
  location = case_when(
    (location %in% normalised_locations$location) == TRUE ~ location,
    (location %in% normalised_locations$location) == FALSE ~ "Other"),
  .keep = "unused") %>% 
  ungroup() %>% 
  mutate(across(c(3, 12:13, 16, 20), as.factor))

# one row per selection per session
age_groups <- multichoice_splitting(df, what_was_the_target_age_group_for_this_session, age_group)
target_groups <- multichoice_splitting(df, which_of_these_groups_was_the_session_designed_to_benefit, target_group)
realm_languages <- multichoice_splitting(df, which_language_s_was_the_session_delivered_in, realm_language)

source("outcomes.R")

source("delivery_agents.R")
