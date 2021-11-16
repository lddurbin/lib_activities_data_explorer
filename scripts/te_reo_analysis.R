library("ggthemes")
library("waffle")
library("ggtext")

te_reo_data <- readRDS(here::here("data/joined_data.rds")) %>%
  filter(realm_language == "Te reo Māori") %>% 
  mutate(local_board = case_when(
    is.na(as.character(in_which_local_board_was_the_session_delivered)) ~ "Online",
    TRUE ~ as.character(in_which_local_board_was_the_session_delivered)
  )) 

kiaora_delivery_agents <- te_reo_data %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(external_delivery = case_when(
    CC_staff_agents == FALSE & non_CC_staff_agents == FALSE ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  count(external_delivery, name = "sessions") %>% 
  mutate(perc = round(sessions/sum(sessions)*100), external_delivery = case_when(
    external_delivery ~ "No staff involved in delivery",
    !external_delivery ~ "Staff involved in delivery",
  )) %>%
  select(-sessions) %>% 
  arrange(perc) %>% 
  deframe()

delivery_agents_waffle <- kiaora_delivery_agents %>% 
  waffle(
    colors = c("blue", "grey"),
    legend_pos = "bottom",
    title = paste0("Between 1 July and ", format(today(), "%d %B"), " 2021, <strong>", kiaora_delivery_agents["No staff involved in delivery"], "%</strong> of te reo Māori<br>Libraries Programmes and Events were delivered without <br>staff involvement"),
    xlab = paste0("1 square = 1% of Libraries programmes and events where more than half of the session was delivered in te reo Māori")
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

te_reo_by_location <- te_reo_data %>% 
  rowwise() %>% 
  mutate(
    participants_tamariki = sum(across(starts_with("how_many_children")), na.rm = TRUE),
    participants_adult = sum(across(starts_with("how_many_adults")), na.rm = TRUE),
    participants_total = case_when(
      !is.na(how_many_people_watched_the_live_online_broadcast_of_this_session) ~ how_many_people_watched_the_live_online_broadcast_of_this_session,
      TRUE ~ sum(participants_tamariki + participants_adult)
    )
    ) %>% 
  ungroup() %>% 
  distinct(id, location, local_board, participants_tamariki, participants_adult, participants_total) %>% 
  group_by(local_board, location) %>% 
  summarise(sessions = n(), participants_tamariki = sum(participants_tamariki), participants_adult = sum(participants_adult), participants_total = sum(participants_total)) %>% 
  arrange(desc(sessions)) %>% 
  adorn_totals()

writexl::write_xlsx(te_reo_by_location, here::here("data/te_reo_analysis.xlsx"))