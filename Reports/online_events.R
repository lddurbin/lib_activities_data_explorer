library(ggtext)
library(waffle)

# 137 online sessions since 1 July 2021, 128 since 17 August 2021
online_sessions <- joined_data %>% 
  filter(online == TRUE)

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id) %>% 
  nrow()

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, across(ends_with("online_broadcast_of_this_session"))) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:4), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  adorn_totals()

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  adorn_totals() %>% 
  tail(1) %>% 
  mutate(duration_hours = round(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour/60))

data <- online_sessions %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(date = as.Date(delivery_datetime) %>% floor_date(unit = "week", week_start = 1)) %>% 
  count(date, name = "sessions")


# Sessions over time ------------------------------------------------------

level_four <- interval(ymd("2021-08-17"), ymd("2021-09-21"))
level_three <- interval(ymd("2021-09-21"), today())

ggplot(mapping = aes(x = data$date, y = data$sessions)) +
  annotate("rect", xmin = as.Date(int_start(level_four)), xmax = as.Date(int_end(level_four)), ymin = 0, ymax = max(data$sessions), fill = "#a93226", alpha = .3) +
  annotate("rect", xmin = as.Date(int_start(level_three)), xmax = as.Date(int_end(level_three)), ymin = 0, ymax = max(data$sessions), fill = "#ff9f33", alpha = .3) +
  geom_line(size = 1.5, color = "blue") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  scale_x_date(date_labels = "%B", breaks = "1 month", limits = c(ymd("2021-06-28"), today())) +
  scale_y_continuous(breaks = c(5,10,15,20,25)) +
  labs(
    title = "More programmes, events, and Book a Librarian sessions<br>were delivered during <span style='color:#a93226'>Alert Level 4</span> than previously, but the<br>activity has fallen back during <span style='color:#ff9f33'>Alert Level 3</span>",
    subtitle = "Number of online sessions per week recorded via the Programmes and Events form since 1 July 2021"
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1))


# Sessions per team -------------------------------------------------------

# 94 sessions after 17 August where CC staff were involved in delivery (only 1 where more than one team collaborated).
lockdown_delivery_teams <- online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  mutate(delivery_library_names = case_when(
    is.na(delivery_library_names) & !is.na(unit_3_teams) ~ unit_3_teams,
    TRUE ~ delivery_library_names
  )) %>% 
  distinct(id, delivery_library_names) %>%
  count(delivery_library_names, sort = TRUE) %>%
  filter(!is.na(delivery_library_names)) %>% 
  mutate(highlight = case_when(
    n > 5 ~ TRUE,
    TRUE ~ FALSE
  ))

ggplot(mapping = aes(x = reorder(lockdown_delivery_teams$delivery_library_names, lockdown_delivery_teams$n), y = lockdown_delivery_teams$n, fill = lockdown_delivery_teams$highlight)) +
  geom_col() +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.text.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("grey", "red")) +
  geom_text(aes(label = lockdown_delivery_teams$n),  hjust = -0.3, colour = "black") +
  labs(
    title = "Libraries staff have delivered 94 online activities<br>during lockdown. Nearly 80% of these<br>sessions were delivered by just <span style='color:red'>six teams</span>",
    subtitle = "Number of online sessions recorded via the Programmes and Events\nform that were delivered after 17 August 2021, and in which Connected\nCommunities staff were involved in delivery"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

# Sessions vs participants per team -------------------------------------------------------

sessions_participants <- online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  mutate(delivery_library_names = case_when(
    is.na(delivery_library_names) & !is.na(unit_3_teams) ~ unit_3_teams,
    TRUE ~ delivery_library_names
  )) %>% 
  distinct(id, delivery_library_names, duration = what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, across(starts_with("how_many"))) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:6), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  filter(!is.na(delivery_library_names)) %>% 
  group_by(delivery_library_names) %>% 
  summarise(sessions = n(), total_participants = sum(total_participants), total_duration = round(sum(duration)/60)) %>% 
  mutate(alpha = case_when(
    sessions > 5 | total_participants > 25 ~ 1,
    TRUE ~ 0
  ))

# Research Central had one particularly popular event with 130 participants: https://www.eventfinda.co.nz/2021/the-memories-in-time-project-with-fiona-brooker/auckland
ggplot(sessions_participants, aes(x=sessions, y=total_participants)) +
  geom_point(size=3) +
  geom_text(label=sessions_participants$delivery_library_names, vjust = -1, hjust = 0, alpha = sessions_participants$alpha) +
  geom_vline(xintercept = max(sessions_participants$sessions)/2, color = "blue") +
  geom_hline(yintercept = max(sessions_participants$total_participants)/2, color = "blue") +
  annotate("label", x = 3, y = -10, label = "Fewer sessions, fewer participants", fill = "white") +
  annotate("label", x = 15, y = -10, label = "More sessions, fewer participants", fill = "white") +
  annotate("label", x = 3, y = 160, label = "Fewer sessions, more participants", fill = "white") +
  annotate("label", x = 15, y = 160, label = "More sessions, more participants", fill = "white") +
  scale_x_continuous(limits = c(0,22)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 14)) +
  labs(y = "Participants", x = "Sessions")

#Only 1 of the online events during lockdown was a pre-school activity. Some pre-recorded so not recorded here (Libraries FB)
online_sessions %>%
  filter(id == "3674")


# External delivery -------------------------------------------------------

online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(across(c(1, 32:34))) %>% 
  mutate(
    staff = case_when(
      CC_staff_agents | non_CC_staff_agents ~ TRUE,
      TRUE ~ FALSE
    ),
    staff_and_externals = case_when(
      staff & external_agents ~ TRUE,
      TRUE ~ FALSE
    ),
    externals_only = case_when(
      !staff & external_agents ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  pivot_longer(5:7, names_to = "delivery_agent_type") %>% 
  count(delivery_agent_type, value)

waffle(
  parts = c("Non-staff involvement" = 39,"Staff delivered" = 61), 
  colors = c("red", "grey"),
  title = "<span style='color:red'><strong>Individuals not employed by Auckland Council</strong></span> were involved<br>in delivering <strong>39%</strong> of the 128 online activities during lockdown",
  xlab = "1 square = 1% of activities delivered during lockdown"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))


# Bi-lingual --------------------------------------------------------------

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>%
  distinct(id, realm_language) %>% 
  pivot_wider(names_from = realm_language, values_from = realm_language) %>% 
  clean_names() %>% 
  mutate(bi_lingual = case_when(
    is.na(other) & is.na(mandarin_chinese) & is.na(te_reo_maori) ~ FALSE,
    TRUE ~ TRUE
  )) %>% 
  count(bi_lingual)

waffle(
  parts = c("Bi-lingual" = 44,"Not bi-lingual" = 56), 
  colors = c("red", "grey"),
  title = "<strong>44%</strong> of the 128 online activities delivered during<br>lockdown were <span style='color:red'><strong>bi-lingual sessions</strong></span> ",
  xlab = "1 square = 1% of activities delivered during lockdown"
) +
  theme(plot.title = element_markdown(lineheight = 1.1))
