library(ggtext)
library(waffle)

online_sessions <- readRDS(here::here("data/joined_data.rds")) %>% 
  filter(online == TRUE)

# Number of sessions since 18 August 2021
total_sessions <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id) %>% 
  nrow()

# Total participants since 18 August 2021
total_participants <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, across(ends_with("online_broadcast_of_this_session"))) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:4), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  adorn_totals() %>% 
  tail(1) %>% 
  pull(total_participants)

# Total number of hours of programming since 18 August 2021
total_duration <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  adorn_totals() %>% 
  tail(1) %>% 
  mutate(duration_hours = round(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour/60)) %>% 
  pull(duration_hours)

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
staff_delivered <- online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  mutate(delivery_library_names = case_when(
    is.na(delivery_library_names) & !is.na(unit_3_teams) ~ unit_3_teams,
    TRUE ~ delivery_library_names
  )) %>% 
  distinct(id, delivery_library_names) %>%
  filter(!is.na(delivery_library_names)) %>%
  distinct(id) %>%
  nrow()

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
    title = paste0("Libraries staff have delivered ", staff_delivered, " online activities<br>during lockdown. About two thirds of these<br>sessions were delivered by just <span style='color:red'>six teams</span>"),
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
  mutate(max_participants = max(total_participants), max_sessions = max(sessions)) %>% 
  mutate(
    alpha = case_when(
      sessions >= 5 | total_participants > 50 ~ 1,
      TRUE ~ 0
    ),
    color = case_when(
      sessions < max_sessions/2 & total_participants > max_participants/2 ~ "red",
      sessions > max_sessions/2 & total_participants < max_participants/2 ~ "blue",
      TRUE ~ "black"
    )
  )

# Research Central had one particularly popular event with 130 participants: https://www.eventfinda.co.nz/2021/the-memories-in-time-project-with-fiona-brooker/auckland
ggplot(sessions_participants, aes(x=sessions, y=total_participants)) +
  geom_point(size=3, color=sessions_participants$color) +
  geom_text(label=sessions_participants$delivery_library_names, vjust = -1, hjust = 0, color = sessions_participants$color, alpha = sessions_participants$alpha) +
  geom_vline(xintercept = max(sessions_participants$sessions)/2, color = "blue") +
  geom_hline(yintercept = max(sessions_participants$total_participants)/2, color = "blue") +
  annotate("label", x = 3, y = -10, label = "Fewer sessions, fewer participants", fill = "white") +
  annotate("label", x = 15, y = -10, label = "More sessions, fewer participants", fill = "white") +
  annotate("label", x = 3, y = 160, label = "Fewer sessions, more participants", fill = "white") +
  annotate("label", x = 15, y = 160, label = "More sessions, more participants", fill = "white") +
  scale_x_continuous(limits = c(0,22)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 14)) +
  labs(
    title = "Whilst the Research Central team have delivered to <span style='color:red'>lots of<br>participants across relatively few sessions</span>, other teams<br>have delivered <span style='color:blue'>many more sessions to fewer participants</span>",
    y = "Participants",
    x = "Sessions"
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

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
  parts = c("Non-staff involvement" = 41,"Staff delivered" = 59), 
  colors = c("red", "grey"),
  title = paste0("<span style='color:red'><strong>Individuals not employed by Auckland Council</strong></span> were involved<br>in delivering <strong>41%</strong> of the ", total_sessions, " online activities during lockdown"),
  xlab = "1 square = 1% of activities delivered during lockdown"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

# Who were the external providers?
external_providers <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, session_name = what_was_the_name_of_the_session, external_agent_names) %>% 
  filter(!is.na(external_agent_names))

# On average, are there more participants at sessions involving externals than not?
online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, across(starts_with("how_many_")), external_agents, session_name = what_was_the_name_of_the_session, external_agent_names) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:6), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  with_groups(external_agents, summarise, avg_participants = round(mean(total_participants)))

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
  parts = c("Bi-lingual" = 38,"Not bi-lingual" = 62), 
  colors = c("red", "grey"),
  title = paste0("<strong>38%</strong> of the ", total_sessions, " online activities delivered during lockdown<br>were <span style='color:red'><strong>bi-lingual sessions</strong></span>"),
  xlab = "1 square = 1% of activities delivered during lockdown"
) +
  theme(plot.title = element_markdown(lineheight = 1.1))

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>%
  distinct(id, realm_language) %>% 
  filter(!is.na(realm_language)) %>% 
  count(realm_language) %>% 
  adorn_percentages(denominator = "col") %>% 
  mutate(realm_language = paste0(realm_language, " (", round(n*100), "%)")) %>% 
  treemap::treemap(
    index = "realm_language",
    vSize = "n",
    type = "index",
    title = "Nearly falf of the 65 bi-lingual sessions delivered during this lockdown were in Te reo Māori",
    fontsize.title = 16,
    aspRatio = 1.5
    )


# Session format ----------------------------------------------------------

online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>%
  distinct(id, format = what_was_the_format_of_the_session) %>% 
  count(format) %>% 
  ggplot(aes(x = reorder(format, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.text.x = element_blank(), legend.position = "none", axis.text.y = element_text(size = 12)) +
  geom_text(aes(label = n),  hjust = -0.3, colour = "black", size = 5) +
  labs(
    title = "During lockdown, classes and workshops were the\nmost frequently delivered type of activity"
  )

sessions_participants_formats <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, across(ends_with("online_broadcast_of_this_session")), format = what_was_the_format_of_the_session) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:4), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  group_by(format) %>% 
  summarise(sessions = n(), participants_per_format = sum(total_participants)) %>% 
  mutate(color = case_when(
    format == "Talk" ~ "red",
    format %in% c("Club", "Class or workshop") ~ "blue",
    TRUE ~ "black"
  )) %>% 
  filter(format != "Pre-school activity")

ggplot(sessions_participants_formats, aes(x=sessions, y=participants_per_format)) +
  geom_point(size=3, color = sessions_participants_formats$color) +
  geom_text(label=sessions_participants_formats$format, vjust = -1.5, hjust = 0, color = sessions_participants_formats$color) +
  geom_vline(xintercept = max(sessions_participants_formats$sessions)/2, color = "blue") +
  geom_hline(yintercept = max(sessions_participants_formats$participants_per_format)/2, color = "blue") +
  annotate("label", x = 10, y = 60, label = "Fewer sessions, fewer participants", fill = "white") +
  annotate("label", x = 35, y = 60, label = "More sessions, fewer participants", fill = "white") +
  annotate("label", x = 10, y = 400, label = "Fewer sessions, more participants", fill = "white") +
  annotate("label", x = 35, y = 400, label = "More sessions, more participants", fill = "white") +
  scale_x_continuous(limits = c(0,55)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 14)) +
  labs(
    title = "Whilst the few <span style='color:red'>talks</span> delivered during lockdown were viewed<br>by many people, fewer people were reached across the<br>many more <span style='color:blue'>clubs and classes/workshops</span> that were delivered",
    y = "Participants",
    x = "Sessions"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))
