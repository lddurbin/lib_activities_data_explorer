library("ggthemes")

kiaora_delivery_agents <- outcome_data %>%
  distinct(id, .keep_all = TRUE) %>% 
  mutate(external_delivery = case_when(
    CC_staff_agents == FALSE & non_CC_staff_agents == FALSE ~ TRUE,
    !is.na(id) ~ FALSE
  )) %>% 
  count(external_delivery, name = "sessions", sort = TRUE) %>% 
  mutate(perc = round(sessions/sum(sessions)*100), ) %>%
  select(-sessions)

tibble(
  group = factor(c("Community-led", "Led by Staff"), ordered = TRUE),
  value = c(14, 86)
) %>% 
  deframe() %>% 
  waffle(
    colors = c("blue", "grey"),
    legend_pos = "bottom",
    title = paste0("In July 2021, 14% of the Kia Ora Tāmaki Makaurau\nLibraries Programmes and Events were delivered by\nmembers of the community"),
    xlab = paste0("1 square = 1% of Kia Ora Tāmaki Makaurau programmes and events not (co-)delivered by AC staff")
  )

kiaora_locations <- locations %>% 
  mutate(location = as.character(location)) %>% 
  mutate(participation = participants_adult + participants_children) %>% 
  filter(!location %in% c("Other", "A community-led facility"))

kiaora_locations %>% 
  top_n(n = 10, wt = sessions) %>% 
  ggplot(aes(x= sessions, y= reorder(location, sessions))) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.5, colour = "white") +
  ggtitle("In July 2021, where were most of \nthe Kia Ora Tāmaki Makaurau Libraries\nProgrammes and Events delivered?")

kiaora_locations %>% 
  top_n(n = 10, wt = participation) %>% 
  ggplot(aes(x= participation, y= reorder(location, participation))) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(participation, big.mark = ",")), hjust = 1.5, colour = "white") +
  ggtitle("In July 2021, which sites observed\nthe highest participation across\nthe Kia Ora Tāmaki Makaurau Libraries\nProgrammes and Events delivered there?")

kiaora_locations_agents <- outcome_data %>% 
  select(id, location, CC_staff_agents:external_agents) %>%
  distinct(id, .keep_all = TRUE) %>% 
  mutate(external_delivery = case_when(
    CC_staff_agents == FALSE & non_CC_staff_agents == FALSE ~ TRUE,
    !is.na(id) ~ FALSE
  )) %>% 
  count(location, external_delivery, name = "sessions", sort = TRUE) %>% 
  with_groups(location, mutate, perc = round(sessions/sum(sessions)*100)) %>% 
  with_groups(location, mutate, total_sessions = sum(sessions))

kiaora_locations_agents %>% 
  filter(perc < 100 & external_delivery == TRUE & location != "A community-led facility") %>%
  ggplot(aes(x= perc, y= reorder(location, perc))) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%, (n=", total_sessions, ")")), hjust = 1.05, colour = "white") +
  ggtitle("At 8 sites, staff had no involvement in\nsome of the Kia Ora Tāmaki Makaurau\nsessions delivered there. What % of their\nsessions were community-led?")

kiaora_teams <- placed_based_teams %>% 
  mutate(participation = participants_adult + participants_children) 

kiaora_teams %>% 
top_n(n = 10, wt = sessions) %>% 
  ggplot(aes(x= sessions, y= reorder(delivery_library_names, sessions))) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.5, colour = "white") +
  ggtitle("In July 2021, which teams were\ndelivering most of the Kia Ora\nTāmaki Makaurau Libraries Programmes\nand Events sessions?")

kiaora_offsite <- outcome_data %>% 
  select(id, location, delivery_library_names) %>% 
  mutate(location = as.character(location), location = case_when(
    location == "Pukekohe Library" ~ "Franklin Community Hub",
    location == "Waiuku Library" ~ "Franklin Community Hub",
    location == "Franklin Arts Centre" ~ "Franklin Community Hub",
    location == "Māngere Town Centre Library" ~ "Māngere Town Centre Community Hub",
    location == "Māngere Arts Centre" ~ "Māngere Town Centre Community Hub",
    location == "Central City Library" ~ "Central City Community Hub",
    location == "Ellen Melville Centre" ~ "Central City Community Hub",
    !is.na(location) ~ location
  )) %>% 
  filter(!is.na(delivery_library_names)) %>% 
  mutate(on_site = case_when(
    location == delivery_library_names ~ "Delivered on site",
    location != delivery_library_names ~ "Delivered off site"
  )) %>% 
  distinct(id, delivery_library_names, on_site) %>% 
  count(delivery_library_names, on_site) %>% 
  with_groups(delivery_library_names, mutate, perc = round(n/sum(n)*100)) %>% 
  with_groups(delivery_library_names, mutate, total_sessions = sum(n)) %>% 
  filter(perc < 100)

kiaora_offsite %>% 
  filter(on_site == "Delivered off site") %>% 
ggplot(mapping = aes(x = reorder(delivery_library_names, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%, (n=", total_sessions, ")")), hjust = 1.05, colour = "white") +
  coord_flip() +
  ggtitle("What % of each team's Kia Ora\nTāmaki Makaurau sessions were\ndelivered off site?")

age_groups %>% 
  ggplot(mapping = aes(x = reorder(age_group, sessions), y = sessions)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle("Which age groups were the Kia\nOra Tāmaki Makaurau sessions\ndesigned to benefit?")

regional_themes %>% 
  rename(themes = which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of) %>% 
  ggplot(mapping = aes(x = reorder(themes, sessions), y = sessions)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle("Which regional themes were the Kia\nOra Tāmaki Makaurau sessions\ndelivered in the context of?")

mandarin_submissions <- joined_data %>%
  filter(month(delivery_datetime) == 7 & realm_language == "Mandarin Chinese")

te_reo_submissions <- joined_data %>%
  filter(month(delivery_datetime) == 7 & realm_language == "Te reo Māori")

all_submissions <- joined_data %>% 
  filter(month(delivery_datetime) == 7) %>% 
  distinct(id, local_board = in_which_local_board_was_the_session_delivered) %>% 
  count(local_board, name = "all_sessions")

tibble(
  group = factor(c("Bi-lingual te reo Māori sessions", ""), ordered = TRUE),
  value = c(4, 96)
) %>% 
  deframe() %>% 
  waffle(
    colors = c("blue", "grey"),
    legend_pos = "bottom",
    title = paste0("In July 2021, 4% of the 2,335 reported Libraries Programmes\nand Events made substantial use of te reo Māori"),
    xlab = paste0("1 square = 1% of reported Libraries programmes and events delivered substantially in te reo Māori")
  )

tibble(
  group = factor(c("Bi-lingual Mandarin Chinese sessions", ""), ordered = TRUE),
  value = c(6, 94)
) %>% 
  deframe() %>% 
  waffle(
    colors = c("blue", "grey"),
    legend_pos = "bottom",
    title = paste0("In July 2021, 6% of the 2,335 reported Libraries Programmes\nand Events made substantial use of Mandarin Chinese"),
    xlab = paste0("1 square = 1% of Libraries programmes and events delivered substantially in Mandarin Chinese")
  )

te_reo_local_board <- te_reo_submissions %>% 
  distinct(id, local_board = in_which_local_board_was_the_session_delivered) %>% 
  count(local_board, name = "te_reo_sessions") %>% 
  left_join(all_submissions, by = "local_board") %>% 
  mutate(perc = round(te_reo_sessions/all_sessions*100))

mandarin_local_board <- mandarin_submissions %>% 
  distinct(id, local_board = in_which_local_board_was_the_session_delivered) %>% 
  count(local_board, name = "mandarin_chinese_sessions") %>% 
  left_join(all_submissions, by = "local_board") %>% 
  mutate(perc = round(mandarin_chinese_sessions/all_sessions*100))

te_reo_local_board %>% 
  mutate(local_board = paste0(local_board, " (n=", all_sessions, ")")) %>% 
  ggplot(mapping = aes(x = reorder(local_board, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.1, colour = "white") +
  coord_flip() +
  labs(title = "In 2 Local Boards, more than 10% of\nthe programmes and events delivered\nin July 2021 made substantial use of\nte reo Māori", subtitle = "% of reported sessions per Local Board where\nmore than half the session was\ndelivered in te reo Māori")

mandarin_local_board %>% 
  mutate(local_board = paste0(local_board, " (n=", all_sessions, ")")) %>% 
  ggplot(mapping = aes(x = reorder(local_board, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.1, colour = "white") +
  coord_flip() +
  labs(title = "In 5 Local Boards, more than 10% of\nthe programmes and events delivered\nin July 2021 made substantial use of\nMandarin Chinese", subtitle = "% of reported sessions per Local Board where\nmore than half the session was\ndelivered in Mandarin Chinese")
