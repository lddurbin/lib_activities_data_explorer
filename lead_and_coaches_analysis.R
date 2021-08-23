common_table_headings <- c('Sessions', 'Participants (18+)', 'Participants (under 18)', 'Hours of Delivery')

source("data_prep.R")

local_board_data <- read_csv("data/local_board_groups.csv", col_types = "cdcc")

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
      local_board_cluster == 2 ~ TRUE,
      local_board_cluster != 2 ~ FALSE
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
  filter(!is.na(in_which_local_board_was_the_session_delivered) & month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021 & what_was_the_format_of_the_session != "Book a Librarian") %>% 
  distinct(id, .keep_all = TRUE)

local_board_highlighted <- local_board_overall %>% 
  rename(local_board = in_which_local_board_was_the_session_delivered) %>% 
  get_local_board_data() %>% 
  filter(highlight_local_board == TRUE)


# Stats by Local Board ----------------------------------------------------

local_board_submissions <- local_board_overall %>% 
  count(local_board = in_which_local_board_was_the_session_delivered, name = "sessions", sort = TRUE) 

local_board_summary <- group_and_sum(local_board_overall, local_board, in_which_local_board_was_the_session_delivered) %>% 
  left_join(local_board_submissions, by = "local_board") %>% 
  get_local_board_data() %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

local_board_summary %>% 
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
  count(location, name = "sessions", sort = TRUE) 

location_summary <- group_and_sum(local_board_highlighted, location, location) %>% 
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


# Stats by session format -------------------------------------------------

session_format_submissions <- local_board_highlighted %>%
  count(session_format = what_was_the_format_of_the_session, name = "sessions", sort = TRUE) 

session_format_summary <- group_and_sum(local_board_highlighted, session_format, what_was_the_format_of_the_session) %>% 
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
  count(outcome, name = "sessions", sort = TRUE) 

outcome_summary <- group_and_sum(local_board_highlighted, outcome, outcome) %>% 
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
  count(language = realm_language, name = "sessions", sort = TRUE) 

language_summary <- group_and_sum(local_board_highlighted, language, realm_language) %>% 
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
