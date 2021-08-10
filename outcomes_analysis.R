source("data_prep.R")

joined_data <- plyr::join_all(list(base_table, outcomes, age_groups, delivery_agents, realm_languages, target_groups), "id") %>% 
  as_tibble()

# How many sessions were entered into the form?
joined_data %>% 
  distinct(id, .keep_all = TRUE) %>% 
  nrow()

# Filter down to an outcome area and de-duplicate on id
outcome_data <- joined_data %>% 
  filter(outcome == "Talanoa") %>% 
  distinct(id, .keep_all = TRUE)

# Retrieve summary data
# summarise_data <- function(df) {
#   df %>% group_by(outcome) %>% 
#     summarise(
#       sessions = n(),
#       participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
#       participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
#       duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
#     )
# }

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

# How many sessions delivered against a given outcome area, how many people attended them, and how many hours were spent delivering them?
outcome_data %>% 
  group_by(outcome) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
    )

# How many sessions delivered against a given outcome?
outcome_data %>% 
  group_by(outcome_attribute) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# In which Local Boards were these sessions delivered, to how many people, and for how long?
outcome_data %>% 
  group_by(in_which_local_board_was_the_session_delivered) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# Where were these sessions delivered, to how many people, and for how long?
outcome_data %>% 
  group_by(location) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# Which sorts of agents delivered them?
outcome_data %>% 
  select(id, CC_staff_agents:external_agents, how_many_adults_aged_18_attended_in_person_at_this_session, how_many_children_aged_under_18_attended_in_person_at_this_session, what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  pivot_longer(CC_staff_agents:external_agents, names_to = "agent_type") %>% 
  filter(value == TRUE) %>% 
  group_by(agent_type) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# Which placed-based teams delivered them?
outcome_data %>% 
  filter(!is.na(delivery_library_names)) %>% 
  group_by(delivery_library_names) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# Which regional teams delivered them?
outcome_data %>% 
  filter(!is.na(unit_names)) %>% 
  group_by(unit_names) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )

# Which age groups were they designed to benefit?
outcome_data %>% 
  filter(!is.na(age_group)) %>% 
  group_by(age_group) %>% 
  summarise(
    sessions = n(),
    participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session),
    participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60
  )
