kai <- c("food|kai|cooking|composting|recipe|gardening|meal|diet|fruit|vegetables|nutrition|whenua")

kai_sessions <- readRDS(here::here("data/joined_data.rds")) %>% 
  select(id, name = what_was_the_name_of_the_session, delivery_datetime, location, comments = 17, ends_with("_this_session")) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  rowwise() %>% 
  mutate(participants = sum(across(ends_with("_this_session")), na.rm = TRUE), .keep = "unused") %>% 
  ungroup() %>% 
  filter(str_detect(str_to_lower(name), kai) | str_detect(str_to_lower(comments), kai))

# write_csv(kai_sessions, here::here("Reports/kai.csv"))
