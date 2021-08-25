common_table_headings <- c('Sessions', 'Participants (18+)', 'Participants (under 18)', 'Hours of Delivery')

source("data_prep.R")

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

local_board_combo <- anti_join(local_board_highlighted, local_board_teams, by = "id") %>%
  mutate(in_which_local_board_was_the_session_delivered = local_board) %>% 
  rename(delivery_team = delivery_library_names) %>% 
  bind_rows(local_board_teams)


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

# Stats by delivery team --------------------------------------------------

delivery_team_submissions <- local_board_teams %>%
  distinct(delivery_team, id, .keep_all = TRUE) %>% 
  count(delivery_team, name = "sessions", sort = TRUE) 

delivery_team_summary <- group_and_sum(local_board_teams %>% distinct(delivery_team, id, .keep_all = TRUE), delivery_team, delivery_team) %>% 
  left_join(delivery_team_submissions, by = "delivery_team") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(delivery_team = case_when(
    is.na(delivery_team) ~ "None",
    !is.na(delivery_team) ~ delivery_team
  )) %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants"))) %>% 
  filter(delivery_team != "None")

delivery_team_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by locations -------------------------------------------------

location_submissions <- local_board_highlighted %>%
  distinct(id, .keep_all = TRUE) %>% 
  count(location, name = "sessions", sort = TRUE) 

location_summary <- group_and_sum(local_board_highlighted %>% distinct(id, .keep_all = TRUE), location, location) %>% 
  left_join(location_submissions, by = "location") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

location_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(location, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


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

session_format_submissions <- local_board_highlighted %>%
  distinct(id, .keep_all = TRUE) %>% 
  count(session_format = what_was_the_format_of_the_session, name = "sessions", sort = TRUE) 

session_format_summary <- group_and_sum(local_board_highlighted %>% distinct(id, .keep_all = TRUE), session_format, what_was_the_format_of_the_session) %>% 
  left_join(session_format_submissions, by = "session_format") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

session_format_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(session_format, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
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

language_submissions <- local_board_highlighted %>%
  distinct(id, realm_language, .keep_all = TRUE) %>% 
  count(language = realm_language, name = "sessions", sort = TRUE) 

language_summary <- group_and_sum(local_board_highlighted %>% distinct(id, realm_language, .keep_all = TRUE), language, realm_language) %>% 
  left_join(language_submissions, by = "language") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(language = case_when(
    is.na(language) ~ "None",
    !is.na(language) ~ language
  )) %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants"))) %>% 
  filter(language != "None")

language_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(language, value, metric), y = value)) +
  geom_col(fill = "blue") +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Sessions by language (all Local Boards) -------------------------------------

all_language_submissions <- local_board_overall %>%
  distinct(id, realm_language, .keep_all = TRUE) %>% 
  count(local_board = in_which_local_board_was_the_session_delivered, language = realm_language, name = "sessions", sort = TRUE) %>% 
  filter(!is.na(language)) %>% 
  get_local_board_data()

all_language_submissions %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(local_board, sessions, language), y = sessions, fill = highlight_local_board)) +
  geom_col() +
  tidytext::scale_x_reordered() +
  facet_wrap("language", scales = "free") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.2, colour = "white") +
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
