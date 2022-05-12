library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggtext)

# Functions ---------------------------------------------------------------

make_plot <- function(metric, is_BaL) {
  change_offline <- metric_changes[[metric]][[1,7]]
  change_online <- metric_changes[[metric]][[2,7]]
  
  change_offline_formatted <- scales::percent(abs(change_offline), big.mark = ",")
  change_online_formatted <- scales::percent(abs(change_online), big.mark = ",")
  
  offline_text <- ifelse(change_offline > 0, paste0(" rose by ", change_offline_formatted, ",<br>but "), paste0(" fell by ", change_offline_formatted, ",<br>and "))
  online_text <- ifelse(change_online > 0, paste0(" rose by ", change_online_formatted), paste0(" fell by ", change_online_formatted))
  
  # offline_text <- paste0(offline_direction, scales::percent(abs(change_offline), big.mark = ","))
  # online_text <- paste0(online_direction, scales::percent(abs(change_online), big.mark = ","))
  
  metric_text <- switch({{metric}}, "duration" = "the hours of ", "participants_adult" = "adult participation at ", "participants_children" = "child participation at ", "sessions" = "the number of ")
  
  plot <- ggplot(data = summary_data, aes(x = month, y = .data[[metric]], colour = location_type)) +
    geom_line(size=2) +
    geom_point(size = 2.5, colour = "black") +
    geom_text(data = tail(summary_data, 2), aes(x = month + days(7), y = .data[[metric]], label = scales::comma(.data[[metric]])), fontface = "bold", hjust = 0.6, vjust = -0.8) +
    scale_colour_manual(values = c("orange", "blue")) +
    scale_x_date(breaks = "1 month", date_labels = "%b") +
    ggthemes::theme_fivethirtyeight() +
    labs(title = 
        paste0(
          "Last month, ", metric_text, "<span style='color:orange'>in-person sessions</span>", offline_text, "remains far below what was reported prior to the Delta outreak.<br>In addition, ", metric_text, "<span style='color:blue'>online sessions</span>", online_text, "."
        )
      ) +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      plot.title = element_markdown(lineheight = 1.1),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot"
    )
  
    ggsave(paste0({{metric}}, ".png"), path = here::here("plots/monthly_summary/"), plot = plot, width = 9, height = 7)
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
  filter(what_was_the_format_of_the_session != "Book a Librarian" & id != "3999") %>% 
  mutate(
    month = as_date(delivery_datetime) %>% floor_date(unit = "months"),
    location_type = case_when(
      in_person == FALSE ~ "Online",
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
  filter(!is.na(month) & month >= ymd("2021-07-01") & month < floor_date(today(), unit = "months"))

labels <- summary_data %>% 
  tail(2)

# Call functions ----------------------------------------------------------

metric_changes <- purrr::map(c("sessions_change", "duration_change", "participants_adult_change", "participants_children_change"), calculate_change)

names(metric_changes) <- c("sessions", "duration", "participants_adult", "participants_children")

purrr::map2(c("sessions", "duration", "participants_adult", "participants_children"), rep(FALSE, 4), make_plot)




BaL_data <- raw_data %>%
  distinct(id, .keep_all = TRUE) %>% 
  filter(what_was_the_format_of_the_session == "Book a Librarian" & id != "3999") %>% 
  mutate(
    month = as_date(delivery_datetime) %>% floor_date(unit = "months"),
    location_type = case_when(
      in_person == FALSE ~ "Online",
      TRUE ~ "In a library"
    )
  ) %>% 
  with_groups(month, summarise, sessions = n_distinct(id)) %>% 
  filter(!is.na(month) & month >= ymd("2021-07-01") & month < floor_date(today(), unit = "months"))

BaL <- ggplot(data = BaL_data, aes(x = month, y = sessions)) +
  geom_area(fill = "#d66060") +
  geom_text(data = filter(BaL_data, row_number()==1 | row_number()==n()), aes(x = month + days(7), y = sessions, label = scales::comma(sessions)), fontface = "bold", hjust = 0.8, vjust = -0.4, size = 6) +
  scale_x_date(breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0,320)) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = 
         paste0("There were ", tail(BaL_data, 1) %>% pull(sessions), " Book-a-Librarian sessions last month, which is<br>still far below what was reported prior to the Delta outreak.")
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size=12),
    axis.text.y = element_blank(),
    plot.title = element_markdown(lineheight = 1.1),
    panel.grid.major.x = element_blank()
  )

ggsave("BaL_sessions.png", path = here::here("plots/monthly_summary/"), plot = BaL, width = 9, height = 7)
