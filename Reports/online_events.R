library(ggtext)

# 133 online sessions since 1 July 2021, 124 since 17 August 2021
online_sessions <- joined_data %>% 
  filter(online == TRUE)

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

# 90 sessions after 17 August where CC staff were involved in delivery (only 1 where more than one team collaborated).
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
    title = "Libraries staff have delivered 90 online activities<br>during lockdown. Nearly 80% of these<br>sessions were delivered by just <span style='color:red'>six teams</span>",
    subtitle = "Number of online sessions recorded via the Programmes and Events\nform that were delivered after 17 August 2021, and in which Libraries\nstaff were involved in delivery"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

#Only 1 of the online events during lockdown was a pre-school activity. Some pre-recorded so not recorded here (Libraries FB)
online_sessions %>%
  filter(id == "3674")

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
