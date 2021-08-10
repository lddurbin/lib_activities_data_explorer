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
    distinct(id, outcome, .keep_all = TRUE) %>% 
    group_by({{group}}) %>%
    summarise(
      sessions = n_distinct(id),
      participants_adult = sum(how_many_adults_aged_18_attended_in_person_at_this_session, na.rm = TRUE),
      participants_children = sum(how_many_children_aged_under_18_attended_in_person_at_this_session, na.rm = TRUE),
      duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour, na.rm = TRUE)/60
    )
}
