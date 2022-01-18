library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggtext)

# Functions ---------------------------------------------------------------

make_plot <- function(metric, save) {
  change_offline <- metric_changes[[metric]][[1,7]]
  change_online <- metric_changes[[metric]][[2,7]]
  
  offline_direction <- ifelse(change_offline > 0, " rose by<br>", " fell by ")
  online_direction <- ifelse(change_online > 0, " rose by ", " fell by ")
  
  offline_text <- paste0(offline_direction, scales::percent(abs(change_offline), big.mark = ","))
  online_text <- paste0(online_direction, scales::percent(abs(change_online), big.mark = ","))
  
  metric_text <- switch({{metric}}, "duration" = "the hours of ", "participants_adult" = "adult participation at ", "participants_children" = "child participation at ", "sessions" = "the number of ")
  
  plot <- ggplot(data = summary_data, aes(x = month, y = .data[[metric]], colour = location_type)) +
    geom_line(size=2) +
    geom_point(size = 2.5, colour = "black") +
    geom_text(data = tail(summary_data, 2), aes(x = month, y = .data[[metric]], label = scales::comma(.data[[metric]])), fontface = "bold", hjust = 0.1, vjust = -1.1) +
    scale_colour_manual(values = c("orange", "blue")) +
    ggthemes::theme_fivethirtyeight() +
    labs(
      title = paste0(
        "Last month, ", metric_text, "<span style='color:orange'>in-person sessions</span>", offline_text, ", and ", metric_text, "<span style='color:blue'>online sessions</span>", online_text, ".<br>As libraries re-opened on 17 November, this is as expected."
        )
      ) +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      plot.title = element_markdown(lineheight = 1.1),
      panel.grid.major.x = element_blank()
    )
  
  if(save) { 
    ggsave(paste0({{metric}}, ".png"), path = here::here("plots/monthly_summary/"), plot = plot, width = 9, height = 7)
    }
}

calculate_change <- function(col_name) {
  metric <- stringr::str_remove({{col_name}}, "_change")
  
  summary_data %>% 
    filter(month > today()-months(3)) %>% 
    with_groups(location_type, mutate, {{col_name}} := (.data[[metric]]-lag(.data[[metric]]))/lag(.data[[metric]])) %>% 
    tail(2)
}

# Prep data ---------------------------------------------------------------

raw_data <- readRDS(here::here("data/joined_data.rds"))

summary_data <- raw_data %>%
  distinct(id, .keep_all = TRUE) %>% 
  filter(what_was_the_format_of_the_session != "Book a Librarian") %>% 
  mutate(
    month = as_date(delivery_datetime) %>% floor_date(unit = "months"),
    location_type = case_when(
      in_person == FALSE ~ "Online",
      # location %in% c("Other", "A community-led facility", "Another Council-managed facility") ~ "Off-site",
      TRUE ~ "In a library"
    )
  ) %>% 
  group_by(month, location_type) %>% 
  summarise(
    sessions = n(),
    duration = sum(what_was_the_duration_of_the_session_to_the_nearest_half_an_hour)/60 %>% round(),
    participants_adult = sum(across(contains(c("adults", "people"))), na.rm = TRUE),
    participants_children = sum(across(contains("children")), na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(!is.na(month) & month > ymd("2021-08-01") & month < floor_date(today(), unit = "months"))

labels <- summary_data %>% 
  tail(2)

# Call functions ----------------------------------------------------------

metric_changes <- purrr::map(c("sessions_change", "duration_change", "participants_adult_change", "participants_children_change"), calculate_change)

names(metric_changes) <- c("sessions", "duration", "participants_adult", "participants_children")

purrr::map2(c("sessions", "duration", "participants_adult", "participants_children"), rep(TRUE, 4), make_plot)
