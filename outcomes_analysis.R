source("data_prep.R")

# How many sessions were entered into the form?
joined_data %>% 
  distinct(id, .keep_all = TRUE) %>% 
  nrow()

# Up to which date?
joined_data %>% 
  select(delivery_datetime) %>% 
  filter(as_date(delivery_datetime) <= today()) %>% 
  arrange(desc(delivery_datetime)) %>% 
  head(1)

# How many sessions delivered against an outcome?
joined_data %>% 
  filter(!is.na(outcome)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  nrow()

# Filter down to an outcome area
outcome_data <- joined_data %>% 
  filter(outcome == "Kia ora Tāmaki Makaurau")

# Stats for sessions that delivered against a given outcome area
outcome_data %>% 
  distinct(id, outcome, .keep_all = TRUE) %>% 
  summarise_data(outcome)

# Stats for sessions that delivered against this outcome area that also delivered against other outcome areas
outcomes %>% 
  filter(outcome != "Literacy") %>% 
  right_join(outcome_data %>% select(-outcome), "id") %>% 
  filter(!is.na(outcome)) %>% 
  distinct(id, outcome, .keep_all = TRUE) %>% 
  summarise_data(outcome)


# Stats for sessions that delivered against a given outcome
outcome_data %>%
  distinct(id, outcome, outcome_attribute, .keep_all = TRUE) %>% 
  group_by(outcome_attribute) %>%
  summarise(
    sessions = n_distinct(id),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session, na.rm = TRUE),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session, na.rm = TRUE),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, na.rm = TRUE)/60
  )

# In which Local Boards were these sessions delivered, to how many people, and for how long?
summarise_data(outcome_data, in_which_local_board_was_the_session_delivered)

# Where were these sessions delivered, to how many people, and for how long?
summarise_data(outcome_data, location)

# Which sorts of agents delivered them?
outcome_data %>% 
  select(id, outcome, CC_staff_agents:external_agents, how_many_adults_aged_18_attended_in_person_at_this_session, how_many_children_aged_under_18_attended_in_person_at_this_session, what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  pivot_longer(CC_staff_agents:external_agents, names_to = "agent_type") %>% 
  filter(value == TRUE) %>% 
  summarise_data(agent_type)

# Which placed-based teams delivered them?
outcome_data %>% 
  filter(!is.na(delivery_library_names)) %>% 
  summarise_data(delivery_library_names)

# Which regional teams delivered them?
outcome_data %>% 
  filter(!is.na(unit_names)) %>% 
  summarise_data(unit_names)
  
# Which age groups were they designed to benefit?
outcome_data %>% 
  filter(!is.na(age_group)) %>% 
  summarise_data(age_group)

# Which other groups were they designed to benefit?
outcome_data %>% 
  filter(!is.na(target_group)) %>% 
  summarise_data(target_group)

# Which non-English languages were they delivered in?
outcome_data %>% 
  filter(!is.na(realm_language)) %>% 
  summarise_data(realm_language)

# Which regional or national event was the session delivered in the context of?
outcome_data %>% 
  filter(!is.na(which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of)) %>% 
  summarise_data(which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of)

# What was the format of the session?
outcome_data %>% 
  filter(!is.na(what_was_the_format_of_the_session)) %>% 
  summarise_data(what_was_the_format_of_the_session)

# remove(list = ls())
# 
# rmarkdown::render("outcomes_analysis.Rmd", params = list(
#   year = 2021,
#   month = "July",
#   outcome = "Environment",
#   outcome_owner = "Kirstin Kane",
#   outcome_url = "https://aklcouncil.sharepoint.com/sites/tools-to-do-my-job/SitePages/Literacy-@-Auckland-Libraries.aspx"
# ),
# envir = new.env())
