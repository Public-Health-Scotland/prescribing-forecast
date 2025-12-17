##########################################################
# shiny-template
# Original author(s): Original author(s)
# Original date: 2025-06-16
# Written/run on RStudio server 2024.9.1.394.7 and R 4.4.2
# Description of content
##########################################################

setwd('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast')

# Get packages
source("shiny/setup.R")

# UI
ui <- secure_app(
  fluidPage(
    lang = "en",
    tagList(
      # Specify most recent fontawesome library - change version as needed
      tags$style("@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),
      navbarPage(
        id = "intabset", # id used for jumping between tabs
        title = div(tags$a(img(src = "phs-logo-white.png", height = 40, alt = "Go to Public Health Scotland (external site)"),
                           href = "https://www.publichealthscotland.scot/",
                           target = "_blank"
        ), # PHS logo links to PHS website
        style = "position: relative; top: -5px;"),
        windowTitle = "Pharmacy Forecast",# Title for browser tab
        header = tags$head(includeCSS("shiny/www/styles.css"),  # CSS stylesheet
                           includeScript("shiny/www/javascript.js"),
                           tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
        ),
        
        ##############################################.
        # NOTES PAGE ----
        ##############################################.
        tabPanel(title = "Notes",
                 icon = icon_no_warning_fn("clipboard"),
                 value = "Notes",
                 
                 source(file.path("shiny/pages/notes/notes_ui.R"), local = TRUE)$value
                 
        ),
        
        # tabPanel(title = "Spotlight",
        #          icon = icon_no_warning_fn("lightbulb"),
        #          value = "Spotlight",
        #          
        #          source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/spotlight/spotlight_ui.R"), local = TRUE)$value
        #          
        # ),
        
        tabPanel(title = "Volume",
                 icon = icon_no_warning_fn("line-chart"),
                 value = "Volume",
                 
                 source(file.path("shiny/pages/volume/volume_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Cost",
                 icon = icon_no_warning_fn("sterling-sign"),
                 value = "Cost",
                 
                 source(file.path("shiny/pages/cost/cost_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Monitoring",
                 icon = icon_no_warning_fn("magnifying-glass-chart"),
                 value = "Monitoring",
                 
                 source(file.path("shiny/pages/trend monitoring/trend_monitoring_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Model Details",
                 icon = icon_no_warning_fn("circle-info"),
                 value = "Model Details",
                 
                 source(file.path("shiny/pages/model details/model_details_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Feedback",
                 icon = icon_no_warning_fn("clipboard-question"),
                 value = "Feedback",
                 
                 source(file.path("shiny/pages/feedback/feedback_ui.R"), local = TRUE)$value
                 
        )
        
      )
    ) 
  )
)


server <- function(input, output, session) {
  
  ##############################################.
  # SHINY MANAGER ----
  ##############################################.
  
  # load in credentials file
  credentials <- readRDS("admin/credentials.rds")
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials),
    # will timeout the app on the server
    timeout = 30
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  ##############################################
  
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
  #source(file.path("shiny/pages/spotlight/spotlight_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/volume/volume_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/cost/cost_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/trend monitoring/trend_monitoring_server.R"), local = TRUE)$value
  
}

# Run the application
shinyApp(ui = ui, server = server)

