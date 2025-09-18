##########################################################
# shiny-template
# Original author(s): Original author(s)
# Original date: 2025-06-16
# Written/run on RStudio server 2024.9.1.394.7 and R 4.4.2
# Description of content
##########################################################


# Get packages
setwd('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny')

source("setup.R")

# UI
ui <- fluidPage(
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
      header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
                         includeScript("www/javascript.js"),
                         tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
      ),
      
      ##############################################.
      # INTRODUCTION PAGE ----
      ##############################################.
      # tabPanel(title = "Introduction",
      #          icon = icon_no_warning_fn("circle-info"),
      #          value = "Introduction",
      #          
      #          source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/accuracy/accuracy_ui.R"), local = TRUE)$value
      #          
      # ),
      # 
      # tabPanel(title = "Results comparison",
      #          icon = icon_no_warning_fn("circle-info"),
      #          value = "Results comparison",
      #          
      #          source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/results/results_ui.R"), local = TRUE)$value
      #          
      # ),
      # 
      # tabPanel(title = "Scotland results",
      #          icon = icon_no_warning_fn("circle-info"),
      #          value = "Scotland results",
      #          
      #          source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/scotland/scotland_results_ui.R"), local = TRUE)$value
      #          
      # ),
      
      tabPanel(title = "Aggregated forecast",
               icon = icon_no_warning_fn("line-chart"),
               value = "Aggregated forecast",
               
               source(file.path("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/pages/final/final_ui.R"), local = TRUE)$value
               
      )
      
    )
  ) 
)

### END OF SCRIPT ###
