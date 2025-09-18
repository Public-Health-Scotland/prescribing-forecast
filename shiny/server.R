server <- function(input, output, session) {
  
  # This chunk stops the app from timing out 
  auto_invalidate <- reactiveTimer(10000)
  observe({
    auto_invalidate()
    cat(".")
  })
  
  # Get functions
  source(file.path("functions/core_functions.R"), local = TRUE)$value
  
  # # Get content for intro and induction pages (key points would probably go here too)
  # source(file.path("pages/intro_page.R"), local = TRUE)$value
  # source(file.path("pages/instructions_page.R"), local = TRUE)$value
  
  
  # Get SERVER code for the data pages
  source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/accuracy/accuracy_server.R"), local = TRUE)$value
  source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/results/results_server.R"), local = TRUE)$value
  source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/scotland/scotland_results_server.R"), local = TRUE)$value
  source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/final/final_server.R"), local = TRUE)$value
  
}


#### End of Script 

