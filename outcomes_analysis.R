# Knit file based on chosen outcome
render_analysis <- function(outcome_data, month, year) {
  rmarkdown::render("outcomes_analysis.Rmd", output_file = paste0("Reports/", outcome_data[4]), params = list(
    year = year,
    month = month,
    outcome = outcome_data[1],
    outcome_owner = outcome_data[2],
    outcome_url = outcome_data[3]
  ),
  envir = new.env())
}

kia_ora <- c("Kia ora Tāmaki Makaurau", " and Judith Waaka", "", "kia_ora")
literacy <- c("Literacy", " and Kirstin Kane", "https://aklcouncil.sharepoint.com/sites/tools-to-do-my-job/SitePages/Literacy-@-Auckland-Libraries.aspx", "literacy")
talanoa <- c("Talanoa", " and Elenoa Mo'a Sili-Mati", "", "talanoa")
belonging <- c("Tātou / Belonging", " and Megan Grimshaw-Jones", "", "belonging")
heritage <- c("Heritage", ", Stacey Smith, and  Jacqueline Snee", "", "heritage")
environment <- c("Environment", "", "", "environment")

walk(list(kia_ora, literacy, talanoa, belonging, heritage, environment), render_analysis, month = "July", year = 2021)
