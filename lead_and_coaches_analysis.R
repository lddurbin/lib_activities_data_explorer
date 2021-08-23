common_table_headings <- c('Sessions', 'Participants (18+)', 'Participants (under 18)', 'Hours of Delivery')

source("data_prep.R")

local_board_data <- read_csv("data/local_board_groups.csv", col_types = "cdcc")

group_and_sum <- function(sum_name, sum_col) {
  local_board_overall %>% 
    group_by(local_board = in_which_local_board_was_the_session_delivered) %>% 
    summarise({{sum_name}} := sum({{sum_col}}, na.rm = TRUE), .groups = "drop")
}

get_local_board_data <- function(df) {
  df %>% 
    left_join(local_board_data, by = "local_board") %>% 
    mutate(highlight_local_board = case_when(
      local_board_cluster == 2 ~ TRUE,
      local_board_cluster != 2 ~ FALSE
    ))
}

monthly_submissions <- joined_data %>% 
  filter(month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021) %>% 
  distinct(id) %>% 
  nrow()

local_board_overall <- joined_data %>% 
  filter(!is.na(in_which_local_board_was_the_session_delivered) & month(as_date(delivery_datetime), label = TRUE, abbr = FALSE) == "July" & year(as_date(delivery_datetime)) == 2021) %>% 
  distinct(id, .keep_all = TRUE)

# Of these, how many were delivered in each Local Board?
local_board_submissions <- local_board_overall %>% 
  count(local_board = in_which_local_board_was_the_session_delivered, name = "sessions", sort = TRUE) 

# For each Local Board, show the total hours of delivery, adult participants, and child participants
local_board_summary <- group_and_sum(total_hours, what_was_the_duration_of_the_session_to_the_nearest_half_an_hour) %>% 
  left_join(group_and_sum(adult_participants, across(starts_with(c("how_many_adults", "how_many_people"))))) %>% 
  left_join(group_and_sum(child_participants, across(starts_with(c("how_many_children", "how_many_people"))))) %>% 
  left_join(local_board_submissions) %>% 
  get_local_board_data() %>% 
  pivot_longer(cols = 2:5, names_to = "metric") %>% 
  mutate(metric = str_replace(metric, "_", " ") %>% str_to_title() %>% factor(levels = c("Sessions", "Total Hours", "Adult Participants", "Child Participants")))

# For each Local Board, show total sessions, hours of delivery, and participants (adults and children), highlighting current Local Board
local_board_summary %>% 
  ggplot(mapping = aes(x = tidytext::reorder_within(local_board, value, metric), y = value, fill = highlight_local_board)) +
  geom_col() +
  tidytext::scale_x_reordered() +
  facet_wrap("metric", scales = "free") +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label = prettyNum(value, big.mark = ",")), hjust = 1.2, colour = "white") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip()


