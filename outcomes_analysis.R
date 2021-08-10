source("data_prep.R")

joined_data <- plyr::join_all(list(base_table, outcomes, age_groups, delivery_agents, realm_languages, target_groups), "id") %>% 
  as_tibble()

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
  filter(outcome == "Literacy")

# How many sessions delivered against a given outcome area, how many people attended them, and how many hours were spent delivering them?
outcome_data %>% 
  distinct(id, outcome, .keep_all = TRUE) %>% 
  summarise_data(outcome)

# How many sessions delivered against a given outcome?
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
