library("ggthemes")

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
      local_board_region == "S" ~ TRUE,
      local_board_region != "S" ~ FALSE
    ))
}

get_teams_data <- function(df) {
  df %>% 
    left_join(delivery_team_data, by = "delivery_team") %>% 
    left_join(local_board_data, by = "local_board") %>% 
    mutate(highlight_local_board = case_when(
      local_board_region == "S" ~ TRUE,
      local_board_region != "S" ~ FALSE
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
  filter(!metric %in% c("Sessions", "Total Hours")) %>%
  ggplot(mapping = aes(x = tidytext::reorder_within(local_board, value, metric), y = value, fill = highlight_local_board)) +
  geom_col() +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.1, colour = "black") +
  theme_wsj()+
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()


# Stats by locations -------------------------------------------------

local_board_highlighted %>%
  distinct(id, .keep_all = TRUE) %>% 
  summarise_data(local_board_region)

location_submissions <- local_board_highlighted %>%
  distinct(id, .keep_all = TRUE) %>% 
  mutate(external_delivery = case_when(
    CC_staff_agents == FALSE & non_CC_staff_agents == FALSE ~ TRUE,
    !is.na(id) ~ FALSE
  )) %>% 
  count(location, external_delivery, name = "sessions", sort = TRUE) %>% 
  mutate(location = as.character(location), location = case_when(
    location == "Central City Library" ~ "Central City Community Hub",
    location == "Ellen Melville Centre" ~ "Central City Community Hub",
    !is.na(location) ~ location
  )) %>% 
  left_join(delivery_team_data, by = c("location" = "delivery_team")) %>% 
  with_groups(location, mutate, perc = round(sessions/sum(sessions)*100)) %>% 
  filter(!is.na(local_board)) %>% 
  mutate(location = str_remove_all(location, " Library| Community Hub"))

location_submissions %>% 
  select(-sessions) %>% 
  filter(perc < 100 & external_delivery == TRUE) %>%
  ggplot(aes(x= perc, y= reorder(location, perc))) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.2, colour = "white") +
  ggtitle("What % of the sessions at each site were\nnot delivered by Auckland Council staff?")


# Number of Children's/Adult Sessions vs Child/Adult Participants (bar charts)  --------------------------------------------------

delivery_team_data_by_age_group <- local_board_teams %>%
  select(id, delivery_team, starts_with(c("how_many_", "what_was_the_duration_"))) %>% 
  left_join(concat_rows(local_board_teams, age_group), by = "id")

delivery_team_adult_sessions <- delivery_team_data_by_age_group %>% 
  filter(!str_detect(str_to_lower(age_group), "youth") & !str_detect(str_to_lower(age_group), "children")) %>% 
  distinct(id, .keep_all = TRUE)

delivery_team_children_sessions <- delivery_team_data_by_age_group %>% 
  filter(str_detect(str_to_lower(age_group), "children")) %>% 
  distinct(id, .keep_all = TRUE)

delivery_team_adult_sessions_summary <- group_and_sum(delivery_team_adult_sessions, delivery_team, delivery_team) %>% 
  left_join(delivery_team_adult_sessions %>% count(delivery_team, name = "sessions", sort = TRUE), by = "delivery_team") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

delivery_team_children_sessions_summary <- group_and_sum(delivery_team_children_sessions, delivery_team, delivery_team) %>% 
  left_join(delivery_team_children_sessions %>% count(delivery_team, name = "sessions", sort = TRUE), by = "delivery_team") %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

delivery_team_children_sessions_summary %>%
  filter(metric == "Sessions") %>%
  ggplot(mapping = aes(x = reorder(delivery_team, value), y = value)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle("Number of Children's Sessions")

delivery_team_children_sessions_summary %>%
  filter(metric == "Child Participants") %>%
  ggplot(mapping = aes(x = reorder(delivery_team, value), y = value)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle("Number of Child Participants")


# Number of Children's/Adult Sessions vs Child/Adult Participants (slope charts)  --------------------------------------------------

slope_chart_children <- delivery_team_children_sessions_summary %>%
  filter(metric %in% c("Sessions", "Child Participants")) %>% 
  left_join(delivery_team_data, by = "delivery_team") %>% 
  arrange(desc(value), metric, delivery_team) %>% 
  with_groups(c(metric), mutate, rank = rank(value, ties.method = "first")) %>% 
  select(-c(value:local_board)) %>%
  pivot_wider(names_from = metric, values_from = rank, values_fill = 0) %>%
  clean_names() %>% 
  mutate(change = case_when(
    child_participants - sessions > 3 ~ "positive",
    child_participants - sessions < -3 ~ "negative",
    !is.na(delivery_team) ~ "none"
  )) %>% 
  pivot_longer(cols = child_participants:sessions, names_to = "metric", values_to = "rank") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title()) %>% 
  left_join(delivery_team_children_sessions_summary, by = c("delivery_team", "metric")) %>% 
  mutate(delivery_team = str_remove_all(delivery_team, " Library| Community Hub"))

slope_chart_children %>% 
  ggplot(aes(x = reorder(metric, desc(metric)), y = rank, group = delivery_team)) +
  geom_line(aes(color = change, alpha = 1), size = 2) +
  scale_color_manual(values = c("negative" = "red", "none" = "grey", "positive" = "blue" )) +
  geom_point(size = 3) +
  geom_text(data = slope_chart_children %>% filter(metric == "Sessions"),
            aes(label = paste0(delivery_team, " (", (value), ")")) ,
            hjust = 1.1,
            fontface = "bold",
            size = 3.5) +
  geom_text(data = slope_chart_children %>% filter(metric == "Child Participants"),
            aes(label = paste0(delivery_team, " (", (value), ")")) ,
            hjust = -.3,
            fontface = "bold",
            size = 3.5) +
  theme_wsj(base_size = 10)+
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
  labs(title = "Even though it didn't deliver the most number of children's sessions\nduring July, Manurewa had the highest number of children\nparticipating across these sessions")

# What % of sessions are they delivering onsite vs offsite?
library("ggtext")

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

delivery_team_summary_comparison %>% 
  filter(metric == "Sessions" & on_site == "Delivered on site") %>% 
  ggplot(mapping = aes(x = reorder(delivery_team, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle("Sessions Delivered On Site")


# What % of their total attendees under-18 are turning up to their offsite sessions?
# delivery_team_summary_comparison %>% 
#   filter(metric == "Child Participants" & perc < 100 & perc > 0) %>% 
#   arrange(delivery_team, (on_site)) %>% 
#   with_groups(delivery_team, mutate, label_y = cumsum(perc) - 0.5 * perc) %>% 
#   ggplot(mapping = aes(x = delivery_team, y = perc, fill = reorder(on_site, desc(on_site)))) +
#   geom_col() +
#   scale_fill_manual(values = c("grey", "orange")) +
#   geom_text(aes(y = label_y, label = paste0(perc, "%")), colour = "black") +
#   labs(
#     title = "<span style='font-size:16pt';>In **July 2021**, what % of the children at each team's sessions are turning up to their <span style='color:orange';>**off site**</span style> and <span style='color:grey';>**on site**</span> sessions?</span>"
#   ) +
#   theme(
#     text = element_text(family = "Times"),
#     plot.title.position = "plot",
#     plot.title = element_markdown(size = 11, lineheight = 1.2),
#     legend.title = element_text(size = 0),
#     axis.title = element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank()
#   ) +
#   coord_flip() +
#   scale_y_discrete() +
#   scale_x_discrete(limits=rev)
# 
# # What % of their total attendees over 18 are turning up to their offsite sessions?
# delivery_team_summary_comparison %>% 
#   filter(metric == "Adult Participants" & perc < 100 & perc > 0) %>% 
#   arrange(delivery_team, (on_site)) %>% 
#   with_groups(delivery_team, mutate, label_y = cumsum(perc) - 0.5 * perc) %>% 
#   ggplot(mapping = aes(x = delivery_team, y = perc, fill = reorder(on_site, desc(on_site)))) +
#   geom_col() +
#   scale_fill_manual(values = c("grey", "orange")) +
#   geom_text(aes(y = label_y, label = paste0(perc, "%")), colour = "black") +
#   labs(
#     title = "<span style='font-size:16pt';>In **July 2021**, what % of the adults at each team's sessions are turning up to their <span style='color:orange';>**off site**</span style> and <span style='color:grey';>**on site**</span> sessions?</span>"
#   ) +
#   theme(
#     text = element_text(family = "Times"),
#     plot.title.position = "plot",
#     plot.title = element_markdown(size = 11, lineheight = 1.2),
#     legend.title = element_text(size = 0),
#     axis.title = element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank()
#   ) +
#   coord_flip() +
#   scale_y_discrete() +
#   scale_x_discrete(limits=rev)


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
  with_groups(delivery_team, mutate, rank = row_number()) %>% 
  filter(rank == 1) %>% 
  arrange(session_format) %>% 
  mutate(session_format = case_when(
    session_format == "Class or workshop" ~ "classes and workshops",
    session_format == "Club" ~ "clubs",
    session_format == "Community event" ~ "community events",
    session_format == "Pre-school activity" ~ "pre-school activities",
    session_format == "Other" ~ "something that doesn't fit the usual categories"
  )) %>% 
  filter(session_format == "something that doesn't fit the usual categories")

session_format_ranking %>% 
  ggplot(mapping = aes(x = reorder(delivery_team, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle(paste0("Which teams were mostly delivering\n", session_format_ranking$session_format, "?"))


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

bi_lingual_submissions <- local_board_teams %>%
  distinct(id, .keep_all = TRUE) %>% 
  mutate(bi_lingual = case_when(
    is.na(realm_language) ~ FALSE,
    !is.na(realm_language) ~ TRUE
  )) %>% 
  count(bi_lingual, delivery_team, name = "sessions") %>% 
  with_groups(delivery_team, mutate, perc = round(sessions/sum(sessions)*100))
  
bi_lingual_submissions %>% 
  filter(bi_lingual == TRUE) %>% 
  ggplot(mapping = aes(x = reorder(delivery_team, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle(paste0("What % of each team's sessions\nare bi-lingual?"))

language_submissions <- local_board_teams %>%
  distinct(id, realm_language, .keep_all = TRUE) %>% 
  count(language = realm_language, delivery_team, name = "sessions", sort = TRUE) %>% 
  mutate(language = case_when(
    is.na(language) ~ "English",
    !is.na(language) ~ language
  )) %>% 
  with_groups(c(delivery_team), mutate, perc = round(sessions/sum(sessions)*100)) %>% 
  filter(perc < 100 & language == "Other")

language_submissions %>% 
  ggplot(mapping = aes(x = reorder(delivery_team, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle(paste0("What % of each team's sessions\nare delivered to a substantial\ndegree in ", language_submissions$language, "?"))


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

age_groups <- delivery_team_adult_sessions_summary %>% 
  bind_rows(delivery_team_children_sessions_summary, .id = "audience") %>% 
  mutate(audience = case_when(
    audience == "1" ~ "adult",
    audience == "2" ~ "children",
  )) %>% 
  filter(metric == "Sessions") %>% 
  select(sessions = value, -metric, everything()) 

age_groups <- local_board_teams %>% 
  distinct(delivery_team, id) %>% 
  count(delivery_team, name = "sessions_total") %>% 
  left_join(age_groups, by = "delivery_team") %>% 
  mutate(perc = round(sessions/sessions_total*100))

age_groups %>%
  filter(audience == "children") %>% 
  ggplot(mapping = aes(x = reorder(delivery_team, perc), y = perc)) +
  geom_col(fill = "blue") +
  theme_wsj(base_size = 9)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  geom_text(aes(label = paste0(perc, "%")), hjust = 1.5, colour = "white") +
  coord_flip() +
  ggtitle(paste0("What % of the sessions delivered\nby each team were designed for\nchildren (aged under 13)?"))


# age_group_submissions <- local_board_teams %>%
#   distinct(id, age_group, .keep_all = TRUE) %>% 
#   count(delivery_team, age_group, name = "sessions", sort = TRUE) %>% 
#   mutate(age_group = case_when(
#     is.na(age_group) ~ "All ages",
#     !is.na(age_group) ~ age_group
#   )) %>% 
#   mutate(age_group = factor(age_group, levels = c("Pre-school children (under 5 years)", "Primary school children (5-12)", "Youths (13-24)", "Adults (25-64)", "Seniors (65+)", "All ages")))
# 
# age_group_submissions %>% 
#   ggplot(mapping = aes(x = tidytext::reorder_within(delivery_team, sessions, age_group), y = sessions)) +
#   geom_col(fill = "blue") +
#   tidytext::scale_x_reordered() +
#   facet_wrap("age_group", scales = "free") +
#   geom_text(aes(label = prettyNum(sessions, big.mark = ",")), hjust = 1.2, colour = "white") +
#   theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
#   coord_flip()


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
