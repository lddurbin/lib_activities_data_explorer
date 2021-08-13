# Knit file based on chosen outcome
render_analysis <- function(outcome_data, month, year) {
  rmarkdown::render("outcomes_analysis.Rmd", output_file = paste0("Reports/", outcome_data[4]), params = list(
    year = year,
    month = month,
    outcome = outcome_data[1],
    outcome_owner = outcome_data[2],
    outcome_url = outcome_data[3],
    outcome_filename = outcome_data[4]
  ),
  envir = new.env())
}

kia_ora <- c("Kia ora Tāmaki Makaurau", " and Judith Waaka", "https://aklcouncil.sharepoint.com/sites/who-we-are/SitePages/maori-outcomes.aspx", "kia_ora")
literacy <- c("Literacy", " and Kirstin Kane", "https://aklcouncil.sharepoint.com/sites/tools-to-do-my-job/SitePages/Literacy-@-Auckland-Libraries.aspx", "literacy")
talanoa <- c("Talanoa", " and Elenoa Mo'a Sili-Mati", "https://aklcouncil.sharepoint.com/sites/who-we-are/SitePages/Pacific-Framework.aspx", "talanoa")
belonging <- c("Tātou / Belonging", " and Megan Grimshaw-Jones", "", "belonging")
heritage <- c("Heritage", ", Stacey Smith, and  Jacqueline Snee", "https://aklcouncil.sharepoint.com/sites/teams-groups/SitePages/heritage-team.aspx", "heritage")
environment <- c("Environment", "", "", "environment")

all_outcomes <- list(kia_ora, literacy, talanoa, belonging, heritage, environment)

purrr::walk(all_outcomes, render_analysis, month = "July", year = 2021)
