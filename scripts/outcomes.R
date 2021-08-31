assign_literacy_attribute <- function(attribute_col, attribute_name) {
  df %>% 
    select(id, outcome_attribute := {{attribute_col}}) %>% 
    filter(outcome_attribute == "Yes") %>% 
    mutate(outcome_attribute := {{attribute_name}})
}

literacy_attributes <- df %>% 
  multichoice_splitting(what_sort_of_literacy_session_was_this, outcome_attribute) %>% 
  mutate(outcome_attribute = case_when(
    outcome_attribute == "Driver's license" ~ "Driver's licence",
    !outcome_attribute %in% unlist(c("Digital", "Employment", "Financial", "Language and numeracy", "Driver's licence", "ESOL", "Pre-school")) ~ "Other",
    !is.na(outcome_attribute) ~ outcome_attribute
  )) %>% 
  bind_rows(map2(c("was_this_an_esol_session", "was_this_literacy_session_designed_for_pre_school_children"), c("ESOL", "Pre-school"), assign_literacy_attribute)) %>% 
  mutate(outcome = "Literacy")

maori_attributes <- multichoice_splitting(df, what_sort_of_kia_ora_tamaki_makaurau_session_was_this, outcome_attribute) %>% 
  mutate(outcome = "Kia ora Tāmaki Makaurau")

pasifika_attributes <- multichoice_splitting(df, what_sort_of_talanoa_session_was_this, outcome_attribute) %>% 
  mutate(outcome = "Talanoa")

outcome_attributes <- bind_rows(literacy_attributes, maori_attributes, pasifika_attributes)

outcomes <- multichoice_splitting(df, which_of_the_following_outcomes_did_the_session_deliver_against, outcome) %>% 
  mutate(outcome = case_when(
    outcome == "Legacy" ~ "Heritage",
    !is.na(outcome) ~ outcome
  )) %>% 
  left_join(outcome_attributes, by = c("id", "outcome"))