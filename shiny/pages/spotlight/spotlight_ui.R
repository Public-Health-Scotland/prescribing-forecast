####################### Summary Page #######################

vbs <- list(
  value_box(
    title = "1st value",
    value = "123",
    showcase = bs_icon("bar-chart"),
    style = "background-color: #3F3685; color: white;",
    p("The 1st detail")
  ),
  value_box(
    title = "2nd value",
    value = "456",
    showcase = bs_icon("graph-up"),
    theme = "teal",
    p("The 2nd detail"),
    p("The 3rd detail")
  ),
  value_box(
    title = "3rd value",
    value = "789",
    showcase = bs_icon("pie-chart"),
    theme = "pink",
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

tagList(
  
  
  tags$head(
    tags$style(HTML("
      .value-box { min-height: 220px; }
      @media (max-width: 992px) { .value-box { min-height: 180px; } }
      @media (max-width: 576px) { .value-box { min-height: 150px; } }
    "))
  ),
  
  ## Heading and introductory text
  fluidRow(
    h1("Spotlight")#,
    #p("Forecasting number of paid items using SARIMA while encorporating important factors such as monthly number of working days and items per prescription."),
    
  ),
  
  # layout_column_wrap(
  #   
  #   value_box( 
  #     title = paste0('Best absolute accuracy margin of Number of Paid Items forecast'), 
  #     value = uiOutput('best_accuracy_items_board'),
  #     showcase = bs_icon("activity"),
  #     style = "background-color: #3F3685; color: white;",
  #     p(uiOutput('best_accuracy_items_number'))
  #     
  #   ),
  #   
  #   value_box(
  #     title = paste0('Best absolute accuracy margin of Gross Ingredient Cost (£) forecast'),
  #     value = uiOutput('best_accuracy_gic_board'),
  #     showcase = bs_icon("bar-chart"),
  #     style = "background-color: #83BB26; color: white;",
  #     p(uiOutput('best_accuracy_gic_number'))
  #   ),
  #   
  #   value_box(
  #     title = paste0('Best absolute accuracy margin of cost per item forecast'),
  #     value = uiOutput('best_accuracy_cpi_board'),
  #     showcase = bs_icon("bar-chart"),
  #     style = "background-color: #83BB26; color: white;",
  #     p(uiOutput('best_accuracy_cpi_number'))
  #   ),
  #   height = 300
  #   
  # ),
  # 
  # showcase_left_center(
  #   width = 0.3,
  #   width_full_screen = "1fr",
  #   max_height = "100px",
  #   max_height_full_screen = 0.67
  # )
  
  layout_column_wrap(
    width = "250px",
    !!!vbs
  )
  
)
