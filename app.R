##########################################################
# shiny-template
# Original author(s): Original author(s)
# Original date: 2025-06-16
# Written/run on RStudio server 2024.9.1.394.7 and R 4.4.2
# Description of content
##########################################################

#setwd('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast')

# Get packages
source("shiny/setup.R")

# UI
ui <- #secure_app(
  
  #theme = my_theme,
  
  
  fluidPage(
    shinyjs::useShinyjs(),
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
        style = "position: relative; top: 0.8em; right: 0.8em; padding-bottom: 0.4em;"),
        windowTitle = "Pharmacy Forecast",# Title for browser tab
        header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
                           #includeScript("shiny/www/javascript.js"),
                           tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
        ),
        
        ##############################################.
        # NOTES PAGE ----
        ##############################################.
        tabPanel(title = "Notes",
                 icon = icon_no_warning_fn("clipboard"),
                 value = "not",
                 
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
                 value = "vol",
                 
                 source(file.path("shiny/pages/volume/volume_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Cost",
                 icon = icon_no_warning_fn("sterling-sign"),
                 value = "cost",
                 
                 source(file.path("shiny/pages/cost/cost_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Comparator",
                 icon = icon_no_warning_fn("code-compare"),
                 value = "comparator",
                 
                 source(file.path("shiny/pages/comparator/comparator_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Monitoring",
                 icon = icon_no_warning_fn("magnifying-glass-chart"),
                 value = "mon",
                 
                 source(file.path("shiny/pages/trend monitoring/trend_monitoring_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Model Details",
                 icon = icon_no_warning_fn("circle-info"),
                 value = "mod",
                 
                 source(file.path("shiny/pages/model details/model_details_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "Feedback",
                 icon = icon_no_warning_fn("clipboard-question"),
                 value = "feed",
                 
                 source(file.path("shiny/pages/feedback/feedback_ui.R"), local = TRUE)$value
                 
        ),
        
        tabPanel(title = "EDA",
                 icon = icon_no_warning_fn("clipboard-question"),
                 value = "eda",
                 
                 source(file.path("shiny/pages/eda/eda_ui.R"), local = TRUE)$value
                 
        )
        
      )
    ) 
  )
#)


server <- function(input, output, session) {
  
  ##############################################
  # Password protection----
  #source(file.path("deployment/protect_app.R"), local = TRUE)$value
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
  source(file.path("shiny/pages/notes/notes_server.R"), local = TRUE)$value
  #source(file.path("shiny/pages/spotlight/spotlight_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/volume/volume_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/cost/cost_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/comparator/comparator_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/trend monitoring/trend_monitoring_server.R"), local = TRUE)$value
  source(file.path("shiny/pages/eda/eda_server.R"), local = TRUE)$value
  
}

# will password protect the app when deployed
# if Protect = TRUE
# if(password_protect){
#   ui <- secure_app(ui)
# }

# Run the application
shinyApp(ui = ui, server = server)

