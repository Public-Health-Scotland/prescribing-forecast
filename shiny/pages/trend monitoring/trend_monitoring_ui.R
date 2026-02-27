####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Monitoring trend"),
    p("This tab allows users to monitor underlying trends that may be influencing the forecast. These are currently number of paid items per prescription, and the number of prescriptions per working day.")#,
    #actionButton('notes', 'Click here for key notes'),
    #p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("monitoring_board", label = 'Choose board:', choices = healthboards, selected = 'SCOTLAND'))
  ),
  
  # fluidRow(
  #   layout_column_wrap(
  #     
  #     value_box( 
  #       title = paste0('Average absolute forecasting error over previous 12 months of observed data measuring number of paid items'), 
  #       value = uiOutput('items_value'),
  #       showcase = bs_icon("activity"),
  #       style = "background-color: #3F3685; color: white;",
  #       p(uiOutput('board'))
  #       
  #   ),
  #   
  #   value_box(
  #     title = paste0('Average absolute forecasting error over previous 12 months of data measuring number of paid items per working day'),
  #     value = uiOutput('items_wd_value'),
  #     showcase = bs_icon("bar-chart"),
  #     style = "background-color: #83BB26; color: white;",
  #     p(uiOutput('board_wd'))
  #   ),
  #   
  #   width = 1/6
  #   
  #   )
  # ),
  
  #fluidRow(p(linebreaks(1))),
  
  navset_pill(
    
    nav_panel('Items per prescription',
              
              fluidRow(    
                p(linebreaks(1)),
                column(10, plotlyOutput('items_pp'))
                ),
                fluidRow(
                  downloadButton("downloadData_items_pp", "Download Data", style = "width:200px;")
              )
              
    ),
    
    nav_panel('Prescriptions per working day',
              
              fluidRow(   
                p(linebreaks(1)),
                column(10, plotlyOutput('pp_wd_plot'))
                ),
              fluidRow(
                downloadButton("downloadData_pp_wd", "Download Data", style = "width:200px;")
              )
              
    )
    
    )
  
)
