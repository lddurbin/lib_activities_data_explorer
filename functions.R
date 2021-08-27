# Split a cell with multi-choice selections into one row per selection
multichoice_splitting <- function(data, multichoice_col, new_col) {
  data %>% 
    select(id, {{multichoice_col}}) %>% 
    mutate({{new_col}} := str_sub({{multichoice_col}}, 2, -2), .keep = "unused") %>% 
    separate_rows({{new_col}}, sep = '","') %>% 
    mutate({{new_col}} := str_replace_all({{new_col}}, '"', '')) %>% 
    filter(!is.na({{new_col}}))
}

# Concatenate multiple rows within each group into a single cell per group
concat_rows <- function(data, col_to_concat) {
  data %>% 
    group_by(id) %>% 
    distinct({{col_to_concat}}) %>% 
    summarise({{col_to_concat}} := str_c({{col_to_concat}}, collapse = " --- "), .groups = "drop")
}

# For a given group, sum the number of rows (i.e. sessions), adult participants, child participants, and duration
summarise_data <- function(df, group) {
  df %>% 
    group_by({{group}}) %>%
    summarise(
      sessions = n_distinct(id),
      participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session, na.rm = TRUE),
      participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session, na.rm = TRUE),
      duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, na.rm = TRUE)/60,
      .groups = "drop"
    )
}

# Create a basic table for the Outcomes report
basic_table <- function(df, first_col, table_caption) {
  DT::datatable(
    df,
    colnames = c(first_col, common_table_headings),
    caption = table_caption,
    options = list(dom = 't'),
    rownames = FALSE
  )
}


# Create a basic faceted bar chart for the Outcomes report
basic_chart <- function(df, dimension) {
  df %>% 
    pivot_longer(2:5, names_to = "metric", values_to = "value") %>% 
    mutate(metric = case_when(
      metric == "sessions" ~ "Sessions",
      metric == "participants_adult" ~ "Participants (18+)",
      metric == "participants_children" ~ "Participants (under 18)",
      metric == "duration" ~ "Total Duration (hours)"
    )) %>% 
    mutate(metric = factor(metric, levels = c("Sessions", "Total Duration (hours)", "Participants (18+)", "Participants (under 18)"))) %>%
    ggplot(mapping = aes(x = tidytext::reorder_within({{dimension}}, value, metric), y = value)) +
    geom_col(fill = "blue") +
    tidytext::scale_x_reordered() +
    facet_wrap("metric", scales = "free") +
    scale_fill_brewer(palette="Dark2") +
    theme(axis.title = element_blank()) +
    coord_flip()
}