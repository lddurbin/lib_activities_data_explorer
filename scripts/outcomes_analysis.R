# Knit file based on chosen outcome
render_analysis <- function(outcome_data, month, year) {
  rmarkdown::render(here::here("Reports/outcomes_analysis.Rmd"), output_file = paste0(here::here("Reports/outcomes/"), outcome_data[4]), params = list(
    year = year,
    month = month,
    outcome = outcome_data[1],
    outcome_owner = outcome_data[2],
    outcome_url = outcome_data[3],
    outcome_file_location = outcome_data[5]
  ),
  envir = new.env())
}

kia_ora <- c("Kia ora Tāmaki Makaurau", " for Judith Waaka", "https://aklcouncil.sharepoint.com/sites/who-we-are/SitePages/maori-outcomes.aspx", "kia_ora", Sys.getenv("KIAORA_URL"))
literacy <- c("Literacy", " for Kirstin Kane", "https://aklcouncil.sharepoint.com/sites/tools-to-do-my-job/SitePages/Literacy-@-Auckland-Libraries.aspx", "literacy", Sys.getenv("LITERACY_URL"))
talanoa <- c("Talanoa", " for Elenoa Mo'a Sili-Mati", "https://aklcouncil.sharepoint.com/sites/who-we-are/SitePages/Pacific-Framework.aspx", "talanoa", Sys.getenv("TALANOA_URL"))
belonging <- c("Tātou / Belonging", " for Megan Grimshaw-Jones", "", "belonging", Sys.getenv("BELONGING_URL"))
heritage <- c("Heritage", " for Stacey Smith, and  Jacqueline Snee", "https://aklcouncil.sharepoint.com/sites/teams-groups/SitePages/heritage-team.aspx", "heritage", Sys.getenv("HERITAGE_URL"))
environment <- c("Environment", "", "", "environment", Sys.getenv("ENVIRONMENT_URL"))

all_outcomes <- list(kia_ora, literacy, talanoa, belonging, heritage, environment)

purrr::walk(all_outcomes, render_analysis, month = "July", year = 2021)
