delivery_agents <- df %>% 
  select(1, 40:42, 44:64) %>%
  select(id, agent_types = 2, external_agent_names = 3, AC_agent_names = 4, unit_names = 21, unit_1_teams = 22, unit_2_teams = 23, unit_3_teams = 24, unit_4_teams = 25, everything()) %>% 
  pivot_longer(10:length(.), values_to = "library_names", values_drop_na = TRUE) %>% 
  mutate(across(c("unit_names", "library_names"), ~str_sub(.x, 2, -2))) %>% 
  separate_rows(c("unit_names", "library_names"), sep = ",") %>% 
  mutate(across(c("unit_names", "library_names"), ~str_sub(.x, 2, -2))) %>% 
  mutate(CC_staff_agents = case_when(
    str_detect(agent_types, "Connected Communities staff") == TRUE ~ TRUE,
    !is.na(agent_types) ~ FALSE
  ),
  non_CC_staff_agents = case_when(
    str_detect(agent_types, "Auckland Council staff") == TRUE ~ TRUE,
    !is.na(agent_types) ~ FALSE
  ),
  external_agents = case_when(
    str_detect(agent_types, "Anyone not employed by Auckland Council") == TRUE ~ TRUE,
    !is.na(agent_types) ~ FALSE
  )
  ) %>% 
  select(-c(agent_types, name))