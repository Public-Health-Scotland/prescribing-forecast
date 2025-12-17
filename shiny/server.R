server <- function(input, output, session) {
  
  # This chunk stops the app from timing out 
  auto_invalidate <- reactiveTimer(10000)
  observe({
    auto_invalidate()
    cat(".")
  })
  
  # Get functions
  source(file.path("shiny/functions/core_functions.R"), local = TRUE)$value
  
  # # Get content for intro and induction pages (key points would probably go here too)
  # source(file.path("pages/intro_page.R"), local = TRUE)$value
  # source(file.path("pages/instructions_page.R"), local = TRUE)$value
  
  
  # Get SERVER code for the data pages
  #source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/spotlight/spotlight_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/volume/volume_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/cost/cost_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/trend monitoring/trend_monitoring_server.R"), local = TRUE)$value
  
}


#### End of Script 

