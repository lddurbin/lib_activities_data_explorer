delivery_agents <- df_1 %>% 
  select(1, 41:43, 45:66) %>%
  select(
    id,
    agent_types = who_delivered_the_session,
    external_agent_names = for_your_own_records_you_can_provide_further_information_about_the_external_delivery_agent_s_in_the_box_below,
    AC_agent_names = for_your_own_records_you_can_enter_the_name_of_the_auckland_council_team_s_in_the_box_below,
    local_board = within_which_local_board_is_the_library_or_community_hub_team_based,
    unit_names = to_which_unit_would_you_assign_the_employee_s_who_delivered_this_session,
    unit_1_teams = for_your_records_you_may_enter_the_name_of_the_team_s_in_the_arts_culture_heritage_unit_who_delivered_the_session_in_the_box_below,
    unit_2_teams = for_your_records_you_may_enter_the_name_of_the_team_s_in_the_community_impact_unit_who_delivered_the_session_in_the_box_below,
    unit_3_teams = for_your_records_you_may_enter_the_name_of_the_team_s_in_the_libraries_and_learning_unit_who_delivered_the_session_in_the_box_below,
    unit_4_teams = for_your_records_you_may_enter_the_name_of_the_team_s_in_the_maori_outcomes_unit_who_delivered_the_session_in_the_box_below,
    everything()) %>% 
  pivot_longer(11:length(.), values_to = "library_names") %>% 
  distinct(id, library_names, .keep_all = TRUE) %>% 
  add_count(id) %>% 
  filter(!is.na(library_names) | n == 1) %>% 
  mutate(across(c("unit_names", "library_names"), ~str_sub(.x, 2, -2))) %>% 
  # mutate(library_names = case_when(
  #   LOCAL BOARDS WITH 1 TEAM ~ paste0(library_names, ",", )
  # ))
  separate_rows(c("unit_names", "library_names"), sep = '","') %>% 
  mutate(across(c("unit_names", "library_names"), ~str_replace_all(.x, '"', ''))) %>% 
  mutate(
    CC_staff_agents = str_detect(agent_types, "Connected Communities staff"),
    non_CC_staff_agents = str_detect(agent_types, "Auckland Council staff"),
    external_agents = str_detect(agent_types, "Anyone not employed by Auckland Council")
    ) %>% 
  select(-c(agent_types, name))
