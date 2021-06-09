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
