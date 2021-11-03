library(ggtext)
library(waffle)

online_sessions <- readRDS(here::here("data/joined_data.rds")) %>% 
  filter(online == TRUE) %>% 
filter(id != "3758") # THIS IS A PRE-RECORDED VIDEO POSTED TO TAKAPUNA LIBRARY'S FACEBOOK PAGE


# Monthly Totals ----------------------------------------------------------

# Number of sessions since 18 August 2021
total_sessions <- online_sessions %>% 
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(id, floor_date(as.Date(delivery_datetime), "months")) %>% 
  nrow()

# Number of sessions per month since 1 July 2021
monthly_sessions <- online_sessions %>% 
  filter(as.Date(delivery_datetime) < ymd("2021-11-01")) %>% 
  distinct(id, month = floor_date(as.Date(delivery_datetime), "months")) %>% 
  with_groups(month, summarise, online_sessions = n())

monthly_sessions %>% 
ggplot(aes(x = month, y = online_sessions)) +
  geom_col(fill = "blue") +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 12)) +
  geom_text(aes(label = scales::comma(online_sessions, accuracy = 1)), vjust = -1, size = 5) +
  labs(
    title = "Number of online sessions per month recorded via the\nLibraries Programmes and Events form"
  )

# Number of participants per month across online sessions recorded since 1 July 2021
monthly_participants <- online_sessions %>% 
  filter(as.Date(delivery_datetime) < ymd("2021-11-01")) %>% 
  distinct(id, delivery_datetime, across(ends_with("online_broadcast_of_this_session"))) %>% 
  rowwise() %>% 
  mutate(total_participants = sum(across(2:4), na.rm = TRUE), month = floor_date(as.Date(delivery_datetime), "months"), .keep = "unused") %>% 
  ungroup() %>% 
  with_groups(month, summarise, participants_per_month = sum(total_participants))

monthly_participants %>% 
  ggplot(aes(x = month, y = participants_per_month)) +
  geom_col(fill = "blue") +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 12)) +
  geom_text(aes(label = scales::comma(participants_per_month, accuracy = 1)), vjust = -1, size = 5) +
  labs(
    title = "Number of participants per month across online sessions\nrecorded via the Libraries Programmes and Events form"
  )


# Total number of hours of programming per month delivered across online sessions since July 1
monthly_duration <- online_sessions %>% 
  filter(as.Date(delivery_datetime) < ymd("2021-11-01")) %>% 
  distinct(id, month = floor_date(as.Date(delivery_datetime), "months"), what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  with_groups(month, summarise, duration_per_month = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)) %>% 
  mutate(monthly_duration_hours = round(duration_per_month/60), .keep = "unused")

monthly_duration %>% 
  ggplot(aes(x = month, y = monthly_duration_hours)) +
  geom_col(fill = "blue") +
  ggthemes::theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 12)) +
  geom_text(aes(label = scales::comma(monthly_duration_hours, accuracy = 1)), vjust = -1, size = 5) +
  labs(
    title = "Number of hours of delivery across online sessions\nrecorded via the Libraries Programmes and Events form"
  )


# Sessions over time ------------------------------------------------------

level_four <- interval(ymd("2021-08-17"), ymd("2021-09-21"))
level_three <- interval(ymd("2021-09-21"), today())

data <- online_sessions %>%
  filter(as.Date(delivery_datetime) <= floor_date(today(), "weeks")) %>% 
  distinct(id, .keep_all = TRUE) %>%
  mutate(date = as.Date(delivery_datetime) %>% floor_date(unit = "week", week_start = 1)) %>% 
  count(date, name = "sessions")

ggplot(mapping = aes(x = data$date, y = data$sessions)) +
  annotate("rect", xmin = as.Date(int_start(level_four)), xmax = as.Date(int_end(level_four)), ymin = 0, ymax = max(data$sessions), fill = "#a93226", alpha = .3) +
  annotate("rect", xmin = as.Date(int_start(level_three)), xmax = as.Date(int_end(level_three)), ymin = 0, ymax = max(data$sessions), fill = "#ff9f33", alpha = .3) +
  geom_line(size = 1.5, color = "blue") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  scale_x_date(date_labels = "%B", breaks = "1 month", limits = c(ymd("2021-06-28"), today())) +
  scale_y_continuous(breaks = c(5,10,15,20,25)) +
  labs(
    title = "More online programmes, events, and Book a Librarian sessions<br>were delivered during <span style='color:#a93226'>Alert Level 4</span> than previously, but the<br>activity has fallen back somewhat during <span style='color:#ff9f33'>Alert Level 3</span>",
    caption = "Number of online sessions per week recorded via the Programmes and Events form since 1 July 2021"
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
    n > 6 ~ TRUE,
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
    title = paste0("Libraries staff have delivered ", staff_delivered, " online activities<br>during lockdown. Two thirds of these<br>sessions were delivered by just <span style='color:red'>six teams</span>"),
    caption = "Number of online sessions recorded via the Programmes and Events form that were delivered\nafter 17 August 2021, and in which Connected Communities staff were involved in delivery"
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
  mutate(max_participants = max(total_participants), max_sessions = max(sessions), delivery_library_names = str_remove_all(delivery_library_names, " Library| Community Hub")) %>% 
  mutate(
    alpha = case_when(
      sessions >= 5 | total_participants > 44 ~ 1,
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
  annotate("label", x = 5, y = -10, label = "Fewer sessions, fewer participants", fill = "white") +
  annotate("label", x = 17, y = -10, label = "More sessions, fewer participants", fill = "white") +
  annotate("label", x = 5, y = 130, label = "Fewer sessions, more participants", fill = "white") +
  annotate("label", x = 17, y = 130, label = "More sessions, more participants", fill = "white") +
  scale_x_continuous(limits = c(0,25)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 14)) +
  labs(
    title = "Whilst the Research Central team have delivered to <span style='color:red'>lots of<br>participants across relatively few sessions</span>, other teams<br>have delivered <span style='color:blue'>more sessions to fewer participants</span>",
    y = "Participants",
    x = "Sessions"
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

#Only a few of the online events during lockdown were pre-school activities. Some pre-recorded so not recorded here (Libraries FB)
online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>%
  filter(str_detect(age_group, "Pre-school")) %>% 
  distinct(id, .keep_all = TRUE)


# External delivery -------------------------------------------------------

delivery_agent_mix <- online_sessions %>%
  filter(as.Date(delivery_datetime) > ymd("2021-08-17")) %>% 
  distinct(across(c(1, 32:34))) %>% 
  mutate(
    staff_only = case_when(
      (CC_staff_agents | non_CC_staff_agents) & !external_agents ~ TRUE,
      TRUE ~ FALSE
    ),
    externals_only = case_when(
      !CC_staff_agents & !non_CC_staff_agents ~ TRUE,
      TRUE ~ FALSE
    ),
    staff_and_externals = case_when(
      external_agents & !externals_only ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  pivot_longer(5:7, names_to = "delivery_agent_type") %>% 
  count(delivery_agent_type, value) %>% 
  filter(value) %>% 
  adorn_percentages(denominator = "col") %>% 
  mutate(n = round(n*100), delivery_agent_type = case_when(
    delivery_agent_type == "staff_only" ~ "Staff-only delivery",
    delivery_agent_type == "externals_only" ~ "External-only delivery",
    delivery_agent_type == "staff_and_externals" ~ "Staff and external co-delivery"
  )) %>% 
  pull(var = n, name = delivery_agent_type)

waffle(
  parts = delivery_agent_mix, 
  colors = c("#066CE6", "#a0cbff", "grey"),
  title = paste0("<strong>", delivery_agent_mix['External-only delivery'], "%</strong> of the ", total_sessions, " online activities delivered during lockdown were<br><span style='color:#066CE6'><strong>conducted without any staff involvement</span>. A further <strong>", delivery_agent_mix['Staff and external co-delivery'], "%</strong><br>were <span style='color:#a0cbff'><strong>delivered jointly by staff and external delivery agents</span>"),
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
  parts = c("Bi-lingual" = 37,"Not bi-lingual" = 63), 
  colors = c("red", "grey"),
  title = paste0("<strong>37%</strong> of the ", total_sessions, " online activities delivered during lockdown<br>were <span style='color:red'><strong>bi-lingual sessions</strong></span>"),
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
    title = "Nearly half of the 65 bi-lingual sessions delivered\nduring this lockdown were in Te reo Māori",
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
    format == "Club" ~ "blue",
    TRUE ~ "black"
  )) %>% 
  filter(format != "Pre-school activity")

ggplot(sessions_participants_formats, aes(x=sessions, y=participants_per_format)) +
  geom_point(size=3, color = sessions_participants_formats$color) +
  geom_text(label=sessions_participants_formats$format, vjust = -1.2, hjust = 0.6, color = sessions_participants_formats$color) +
  geom_vline(xintercept = max(sessions_participants_formats$sessions)/2, color = "blue") +
  geom_hline(yintercept = max(sessions_participants_formats$participants_per_format)/2, color = "blue") +
  annotate("label", x = 17, y = 200, label = "Fewer sessions, fewer participants", fill = "white") +
  annotate("label", x = 65, y = 200, label = "More sessions, fewer participants", fill = "white") +
  annotate("label", x = 17, y = 600, label = "Fewer sessions, more participants", fill = "white") +
  annotate("label", x = 65, y = 600, label = "More sessions, more participants", fill = "white") +
  scale_x_continuous(limits = c(-5,80)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 14)) +
  labs(
    title = "Whilst the few <span style='color:red'>talks</span> delivered during lockdown were viewed<br>by many people, fewer people were reached across the<br>many more <span style='color:blue'>clubs</span> that were delivered",
    y = "Participants",
    x = "Sessions"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))
