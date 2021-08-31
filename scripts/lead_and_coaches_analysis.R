common_table_headings <- c('Sessions', 'Participants (18+)', 'Participants (under 18)', 'Hours of Delivery')

source(here::here("scripts/data_prep.R"))

local_board_data <- read_csv("data/local_board_groups.csv", col_types = "cdcc")

delivery_team_data <- read_csv("data/expected_teams.csv", col_types = "cc")

group_and_sum <- function(df, group_name, group_col) {
  df %>% 
    group_by({{group_name}} := {{group_col}}) %>% 
    summarise(
      total_hours = round(sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, na.rm = TRUE)/60),
      adult_participants = sum(across(starts_with(c("how_many_adults", "how_many_people"))), na.rm = TRUE),
      child_participants = sum(across(starts_with(c("how_many_children", "how_many_people"))), na.rm = TRUE),
      .groups = "drop")
}

get_local_board_data <- function(df) {
  df %>% 
    left_join(local_board_data, by = "local_board") %>% 
    mutate(highlight_local_board = case_when(
      local_board_cluster %in% c(1,6,8) ~ TRUE,
      !local_board_cluster %in% c(1,6,8) ~ FALSE
    ))
}

get_teams_data <- function(df) {
  df %>% 
    left_join(delivery_team_data, by = "delivery_team") %>% 
    left_join(local_board_data, by = "local_board") %>% 
    mutate(highlight_local_board = case_when(
      local_board_cluster %in% c(1,6,8) ~ TRUE,
      !local_board_cluster %in% c(1,6,8) ~ FALSE
    ))
}


# Total submissions -------------------------------------------------------

monthly_submissions_programmes_and_events <- joined_data %>% 
  filter(month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021 & what_was_the_format_of_the_session != "Book a Librarian") %>% 
  distinct(id) %>% 
  nrow()

monthly_submissions_book_a_librarian <- joined_data %>% 
  filter(month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021 & what_was_the_format_of_the_session == "Book a Librarian") %>% 
  distinct(id) %>% 
  nrow()


# Local Board data --------------------------------------------------------

local_board_overall <- joined_data %>% 
  filter(!is.na(in_which_local_board_was_the_session_delivered) & month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021 & what_was_the_format_of_the_session != "Book a Librarian")

local_board_highlighted <- local_board_overall %>% 
  rename(local_board = in_which_local_board_was_the_session_delivered) %>% 
  get_local_board_data() %>% 
  filter(highlight_local_board == TRUE)

local_board_teams <- local_board_overall %>% 
  rename(delivery_team = delivery_library_names) %>% 
  get_teams_data() %>% 
  filter(highlight_local_board == TRUE)


# Stats by Local Board ----------------------------------------------------

local_board_submissions <- local_board_overall %>% 
  distinct(id, .keep_all = TRUE) %>% 
  count(local_board = in_which_local_board_was_the_session_delivered, name = "sessions", sort = TRUE) 

local_board_summary <- group_and_sum(local_board_overall %>% distinct(id, .keep_all = TRUE), local_board, in_which_local_board_was_the_session_delivered) %>% 
  left_join(local_board_submissions, by = "local_board") %>% 
  get_local_board_data() %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

local_board_summary %>% 
  # filter(!metric %in% c("Sessions", "Total Hours")) %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(local_board, value, metric), y = value, fill = highlight_local_board)) +
  geom_col() +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by locations -------------------------------------------------

location_submissions <- local_board_highlighted %>%
  distinct(id, .keep_all = TRUE) %>% 
  count(location, CC_staff_agents, name = "sessions", sort = TRUE) %>% 
  mutate(location = as.character(location), location = case_when(
    location == "Central City Library" ~ "Central City Community Hub",
    location == "Ellen Melville Centre" ~ "Central City Community Hub",
    !is.na(location) ~ location
  )) %>% 
  left_join(delivery_team_data, by = c("location" = "delivery_team")) %>% 
  with_groups(location, mutate, perc = round(sessions/sum(sessions)*100)) %>% 
  filter(!is.na(local_board)) %>% 
  mutate(CC_staff_agents = case_when(
    CC_staff_agents ~ "Yes",
    !CC_staff_agents ~ "No"
  ))

location_submissions %>% 
ggplot(mapping = aes(x = location, y = perc, fill = CC_staff_agents)) +
  geom_bar(aes(fill = CC_staff_agents), position="dodge", stat="identity") +
  facet_wrap("local_board", scales = "free") +
  theme(axis.title = element_blank(), legend.position = "bottom") +
  labs(fill = "Were Connected Communities staff involvement in delivering?")


# Stats by delivery team --------------------------------------------------

delivery_team_submissions_comparison <- local_board_teams %>%
  mutate(delivery_team = case_when(
    delivery_team == "Central City Community Hub" ~ "Central City Library",
    delivery_team != "Central City Community Hub" ~ delivery_team
  )) %>% 
  mutate(on_site = case_when(
    location == delivery_team ~ "Delivered on site",
    location != delivery_team ~ "Delivered off site"
  )) %>% 
  distinct(delivery_team, id, .keep_all = TRUE) %>% 
  count(delivery_team, on_site, name = "sessions", sort = TRUE)

delivery_team_summary_comparison <- local_board_teams %>%
  mutate(delivery_team = case_when(
    delivery_team == "Central City Community Hub" ~ "Central City Library",
    delivery_team != "Central City Community Hub" ~ delivery_team
  )) %>% 
  mutate(on_site = case_when(
    location == delivery_team ~ "Delivered on site",
    location != delivery_team ~ "Delivered off site"
  )) %>%
  distinct(delivery_team, id, .keep_all = TRUE) %>% 
  group_by(delivery_team, on_site) %>% 
  summarise(
    total_hours = round(sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, na.rm = TRUE)/60),
    adult_participants = sum(across(starts_with(c("how_many_adults", "how_many_people"))), na.rm = TRUE),
    child_participants = sum(across(starts_with(c("how_many_children", "how_many_people"))), na.rm = TRUE),
    .groups = "drop") %>% 
  left_join(delivery_team_submissions_comparison, by = c("delivery_team", "on_site")) %>% 
  pivot_longer(cols = 3:6, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants"))) %>% 
  with_groups(c(delivery_team, metric), mutate, perc = round(value/sum(value)*100))

delivery_team_submissions_volumes <- local_board_teams %>%
  distinct(delivery_team, id, .keep_all = TRUE) %>% 
  count(delivery_team, name = "sessions", sort = TRUE) 

delivery_team_summary_volumes <- group_and_sum(local_board_teams %>% distinct(id, .keep_all = TRUE), delivery_team, delivery_team) %>% 
  left_join(delivery_team_submissions_volumes, by = "delivery_team") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

# How many sessions / hours of delivery / participants per delivery team?
delivery_team_summary_volumes %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()

slope_chart_all <- delivery_team_summary_volumes %>%
  filter(metric %in% c("Sessions", "Child Participants")) %>% 
  left_join(delivery_team_data, by = "delivery_team") %>% 
  mutate(local_board = case_when(
    delivery_team == "Central City Library" ~ "Waitematā",
    delivery_team != "Central City Library" ~ local_board
  )) %>% 
  mutate(highlight_team = case_when(
    delivery_team %in% c("Onehunga & Oranga Community Hub", "St Heliers Library") ~ TRUE,
    !delivery_team %in% c("Onehunga & Oranga Community Hub", "St Heliers Library") ~ FALSE,
  )) %>% 
  arrange(desc(value), metric, delivery_team) %>% 
  with_groups(c(metric), mutate, rank = rank(value, ties.method = "first"))

slope_chart_all %>% 
  ggplot(aes(x = metric, y = rank, group = delivery_team)) +
  geom_line(aes(color = highlight_team, alpha = 1), size = 2) +
  scale_color_manual(values = c("grey", "blue")) +
  geom_point(size = 3) +
  geom_text(data = slope_chart_all %>% filter(metric == "Sessions"),
            aes(label = paste0(delivery_team, " (", rev(rank), ")")) ,
            hjust = 1.1,
            fontface = "bold",
            size = 3) +
  geom_text(data = slope_chart_all %>% filter(metric == "Child Participants"),
            aes(label = paste0(delivery_team, " (", rev(rank), ")")) ,
            hjust = -.3,
            fontface = "bold",
            size = 3) +
  theme(legend.position = "none") +
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  theme(axis.ticks       = element_blank()) +
  theme(axis.text.x = element_text(size=14,face="bold")) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  labs(
    title = "Some teams are reaching many under-18s over surprisingly few sessions",
    subtitle = "Ranking Central-East Library & Hub teams by the number of sessions they delivered in July 2021 \nand the number of under-18s who attended them",
  )


library("ggtext")

# What % of sessions are they delivering onsite vs offsite?
delivery_team_summary_comparison %>% 
  filter(metric == "Sessions") %>% 
  arrange(delivery_team, (on_site)) %>% 
  with_groups(delivery_team, mutate, label_y = cumsum(perc) - 0.5 * perc) %>% 
  ggplot(mapping = aes(x = delivery_team, y = perc, fill = reorder(on_site, desc(on_site)))) +
  geom_col() +
  scale_fill_manual(values = c("grey", "orange")) +
  geom_text(aes(y = label_y, label = paste0(perc, "%")), colour = "black") +
  labs(
    title = "<span style='font-size:16pt';>What % of sessions delivered by each team in **July 2021** were held <span style='color:orange';>**off site**</span style> and <span style='color:grey';>**on site**</span>?</span>"
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 11, lineheight = 1.2),
    legend.title = element_text(size = 0),
    axis.title = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  coord_flip() +
  scale_y_discrete() +
  scale_x_discrete(limits=rev)

# What % of their total attendees under-18 are turning up to their offsite sessions?
delivery_team_summary_comparison %>% 
  filter(metric == "Child Participants" & perc < 100 & perc > 0) %>% 
  arrange(delivery_team, (on_site)) %>% 
  with_groups(delivery_team, mutate, label_y = cumsum(perc) - 0.5 * perc) %>% 
  ggplot(mapping = aes(x = delivery_team, y = perc, fill = reorder(on_site, desc(on_site)))) +
  geom_col() +
  scale_fill_manual(values = c("grey", "orange")) +
  geom_text(aes(y = label_y, label = paste0(perc, "%")), colour = "black") +
  labs(
    title = "<span style='font-size:16pt';>In **July 2021**, what % of the children at each team's sessions are turning up to their <span style='color:orange';>**off site**</span style> and <span style='color:grey';>**on site**</span> sessions?</span>"
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 11, lineheight = 1.2),
    legend.title = element_text(size = 0),
    axis.title = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  coord_flip() +
  scale_y_discrete() +
  scale_x_discrete(limits=rev)

# What % of their total attendees over 18 are turning up to their offsite sessions?
delivery_team_summary_comparison %>% 
  filter(metric == "Adult Participants" & perc < 100 & perc > 0) %>% 
  arrange(delivery_team, (on_site)) %>% 
  with_groups(delivery_team, mutate, label_y = cumsum(perc) - 0.5 * perc) %>% 
  ggplot(mapping = aes(x = delivery_team, y = perc, fill = reorder(on_site, desc(on_site)))) +
  geom_col() +
  scale_fill_manual(values = c("grey", "orange")) +
  geom_text(aes(y = label_y, label = paste0(perc, "%")), colour = "black") +
  labs(
    title = "<span style='font-size:16pt';>In **July 2021**, what % of the adults at each team's sessions are turning up to their <span style='color:orange';>**off site**</span style> and <span style='color:grey';>**on site**</span> sessions?</span>"
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 11, lineheight = 1.2),
    legend.title = element_text(size = 0),
    axis.title = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  coord_flip() +
  scale_y_discrete() +
  scale_x_discrete(limits=rev)

slope_chart_offsite_all <- delivery_team_summary_comparison %>%
  filter(metric %in% c("Sessions", "Adult Participants") & on_site == "Delivered off site" & perc < 100) %>% 
  group_by(delivery_team) %>% 
  left_join(delivery_team_data, by = "delivery_team") %>% 
  mutate(local_board = case_when(
    delivery_team == "Central City Library" ~ "Waitematā",
    delivery_team != "Central City Library" ~ local_board
  ))

slope_chart_offsite_all %>% 
  ggplot(aes(x = metric, y = perc, group = delivery_team)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  geom_text(data = slope_chart_offsite_all %>% filter(metric == "Sessions"),
            aes(label = paste0(delivery_team, ": ", perc, "%", " (n=", value, ")")) ,
            hjust = 1.1,
            fontface = "bold",
            size = 3) +
  geom_text(data = slope_chart_offsite_all %>% filter(metric == "Adult Participants"),
            aes(label = paste0(delivery_team, ": ", perc, "%", " (n=", value, ")")) ,
            hjust = -.1,
            fontface = "bold",
            size = 3) +
  facet_wrap("local_board") +
  theme(legend.position = "none") +
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  labs(
    title = "Off-site programmes and events delivered by teams in the Central-East region",
    subtitle = "What % of sessions did they deliver off site, and what % of the adults at all of their sessions attended these off-site sessions?",
  )


# Comet plot for delivery team vs location -------------------------------------------------

# library(extrafont)
# library(hablar)
# library(ggforce)
# library(magick)
# library(ggtext)
# 
# df_comet <- location_summary %>% 
#   rename(value_location = value) %>% 
#   left_join(delivery_team_summary, by = c("location" = "delivery_team", "metric")) %>% 
#   rename(value_team = value) %>% 
#   filter(!is.na(value_team) & !is.na(value_location)) %>% 
#   mutate(team_location = value_team - value_location, pos_neg = case_when(
#     team_location >= 0 ~ "More By Team", 
#     TRUE ~ "More at Location",
#   )) %>% 
#   filter(as.character(metric) == "Child Participants")
# 
# theme_owen <- function () {
#   theme_minimal(base_size=9) %+replace%
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
#     )
# }
# 
# df_comet %>% 
#   ggplot() + 
#   geom_link(aes(x = value_team, y = location, xend = value_location, yend = location, color = pos_neg, size = stat(index))) +
#   scale_color_manual(values = c("#00BFFF", "#00BFFF")) +
#   scale_size(range = c(.01, 4)) + 
#   theme_owen() +
#   geom_point(
#     data = filter(df_comet, team_location > 0),
#     aes(value_location, y = location, color = pos_neg),
#     shape = 21,
#     fill = "white",
#     size = 3.5
#   )  +
#   geom_point(
#     data = filter(df_comet, team_location < 0),
#     aes(value_location, y = location, color = pos_neg),
#     shape = 21,
#     fill = "white",
#     size = 3.5
#   ) + 
#   facet_wrap("metric", scales = "free") +
#   theme(legend.position = 'none', 
#         plot.title.position = 'plot',
#         axis.text.y = element_text(size = 8),
#         plot.title = element_text(face = 'bold', size = 15), 
#         plot.subtitle = element_text(size = 7), 
#         plot.margin = margin(10, 10, 20, 10)) +
#   labs(x = "Number of children who attended sessions delivered by the team or at the site", 
#        y = "", 
#        title = expression(paste("Are More Children Attending Programmes and Events Delivered ", italic("By"), " Libraries/Hubs Staff, or ", italic("at"), " the Libraries/Hubs Sites?")), 
#        subtitle = "How many children are attending programmes and events delivered by teams (tail of comet) vs at the sites where those teams are based (head of comet)?") 


# Stats by session format -------------------------------------------------

session_format_submissions <- local_board_teams %>%
  distinct(id, .keep_all = TRUE) %>% 
  count(delivery_team, session_format = what_was_the_format_of_the_session, name = "sessions", sort = TRUE) %>% 
  with_groups(c(delivery_team), mutate, perc = round(sessions/sum(sessions)*100))

session_format_ranking <- session_format_submissions %>%
  arrange(delivery_team, desc(perc)) %>% 
  with_groups(delivery_team, mutate, rank = row_number())

session_format_ranking %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, perc, session_format), y = perc)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("session_format", scales = "free") +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by outcomes -------------------------------------------------

outcome_submissions <- local_board_highlighted %>%
  distinct(id, outcome, .keep_all = TRUE) %>% 
  count(outcome, name = "sessions", sort = TRUE) 

outcome_summary <- group_and_sum(local_board_highlighted %>% distinct(id, outcome, .keep_all = TRUE), outcome, outcome) %>% 
  left_join(outcome_submissions, by = "outcome") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(outcome = case_when(
    is.na(outcome) ~ "None",
    !is.na(outcome) ~ outcome
  )) %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

outcome_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(outcome, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by languages (this Local Board) ------------------------------------------------------

language_submissions <- local_board_teams %>%
  distinct(id, realm_language, .keep_all = TRUE) %>% 
  count(language = realm_language, delivery_team, name = "sessions", sort = TRUE) %>% 
  mutate(language = case_when(
    is.na(language) ~ "English",
    !is.na(language) ~ language
  )) %>% 
  with_groups(c(delivery_team), mutate, perc = round(sessions/sum(sessions)*100))

# language_summary <- group_and_sum(local_board_highlighted %>% distinct(id, realm_language, .keep_all = TRUE), language, realm_language) %>% 
#   left_join(language_submissions, by = "language") %>% 
#   pivot_longer(cols = 2:5, names_to = "metric") %>% 
#   mutate(language = case_when(
#     is.na(language) ~ "None",
#     !is.na(language) ~ language
#   )) %>% 
#   mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants"))) %>% 
#   filter(language != "None")

language_submissions %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, perc, language), y = perc)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("language", scales = "free") +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Sessions by language (all Local Boards) -------------------------------------

all_language_submissions <- local_board_overall %>%
  distinct(id, realm_language, .keep_all = TRUE) %>% 
  count(local_board = in_which_local_board_was_the_session_delivered, language = realm_language, name = "sessions", sort = TRUE) %>% 
  get_local_board_data() %>% 
  with_groups(c(local_board), mutate, perc = round(sessions/sum(sessions)*100)) %>% 
  filter(!is.na(language))

all_language_submissions %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(local_board, perc, language), y = perc, fill = highlight_local_board)) +
  geom_col() +
  tidytext::scale_x_reordered() +
  facet_wrap("language", scales = "free") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by age group ------------------------------------------------------

age_group_submissions <- local_board_teams %>%
  distinct(id, age_group, .keep_all = TRUE) %>% 
  count(delivery_team, age_group, name = "sessions", sort = TRUE) %>% 
  mutate(age_group = case_when(
    is.na(age_group) ~ "All ages",
    !is.na(age_group) ~ age_group
  )) %>% 
  mutate(age_group = factor(age_group, levels = c("Pre-school children (under 5 years)", "Primary school children (5-12)", "Youths (13-24)", "Adults (25-64)", "Seniors (65+)", "All ages")))

age_group_submissions %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, sessions, age_group), y = sessions)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("age_group", scales = "free") +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by target group ---------------------------------------------------

target_group_submissions <- local_board_teams %>%
  distinct(id, target_group, .keep_all = TRUE) %>% 
  count(delivery_team, target_group, name = "sessions", sort = TRUE) %>% 
  mutate(target_group = case_when(
    is.na(target_group) ~ "None",
    !is.na(target_group) ~ target_group
  )) %>% 
  filter(target_group != "None")

target_group_submissions %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, sessions, target_group), y = sessions)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("target_group", scales = "free") +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()

# Stats by regional theme ---------------------------------------------------

regional_theme_submissions <- local_board_teams %>%
  distinct(id, which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of, .keep_all = TRUE) %>% 
  count(regional_theme = which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of, name = "sessions", sort = TRUE)

regional_theme_summary <- group_and_sum(local_board_highlighted %>% distinct(id, which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of, .keep_all = TRUE), regional_theme, which_of_these_regional_or_national_events_was_the_session_designed_or_delivered_in_the_context_of) %>% 
  left_join(regional_theme_submissions, by = "regional_theme") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(regional_theme = case_when(
    is.na(regional_theme) ~ "None",
    !is.na(regional_theme) ~ regional_theme
  )) %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants"))) %>% 
  filter(regional_theme != "None")

regional_theme_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(regional_theme, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()
