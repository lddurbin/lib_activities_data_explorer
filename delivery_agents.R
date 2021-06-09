delivery_agents <- df %>% 
  select(id, agent_types = who_delivered_the_session, unit_names = to_which_unit_would_you_assign_the_employee_s_who_delivered_this_session, starts_with(c("within_which", "for_your_"))) %>% 
  rename(local_board = 4, external_agent_names = length(.)-5, AC_agent_names = length(.)-4, unit_1_teams = length(.)-3, unit_2_teams = length(.)-2, unit_3_teams = length(.)-1, unit_4_teams = length(.)) %>% 
  pivot_longer(starts_with("within_which_albert"):starts_with("within_which_whau"), values_to = "delivery_library_names") %>% 
  distinct(id, delivery_library_names, .keep_all = TRUE) %>%
  add_count(id) %>%
  filter(!is.na(delivery_library_names) | n == 1) %>% 
  select(-n) %>% 
  mutate(across(c("unit_names", "delivery_library_names"), ~str_sub(.x, 2, -2))) %>% 
  mutate(
    Aotea = case_when(str_detect(local_board, c("Aotea / Great Barrier")) == TRUE ~ '"Great Barrier Library"'),
    Franklin = case_when(str_detect(local_board, c("Franklin")) == TRUE ~ '"Franklin Community Hub"'),
    Puketapapa = case_when(str_detect(local_board, c("Puketapapa")) == TRUE ~ '"Mt Roskill Library"'),
    Upper_Harbour = case_when(str_detect(local_board, c("Upper Harbour")) == TRUE ~ '"Albany Village Library"'),
    Waiheke = case_when(str_detect(local_board, c("Waiheke")) == TRUE ~ '"Waiheke Library"')
  ) %>%
  unite("delivery_library_names", delivery_library_names:Waiheke, sep = ",", remove = TRUE, na.rm = TRUE) %>%
  separate_rows(c("unit_names", "delivery_library_names"), sep = '","') %>%
  mutate(across(c("unit_names", "delivery_library_names"), ~str_replace_all(.x, '"', ''))) %>%
  mutate(
    CC_staff_agents = str_detect(agent_types, "Connected Communities staff"),
    non_CC_staff_agents = str_detect(agent_types, "Auckland Council staff"),
    external_agents = str_detect(agent_types, "Anyone not employed by Auckland Council")
  ) %>%
  select(-c(agent_types, name, local_board)) %>%
  mutate(across(everything(), na_if, ""))
