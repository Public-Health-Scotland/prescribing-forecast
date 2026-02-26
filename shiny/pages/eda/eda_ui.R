####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Exploratory Data Analysis (DO NOT PUBLISH)"),
    p("Tab to explore data using interactive charts.")#,
    #actionButton('notes', 'Click here for key notes'),
    #p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("eda_board", label = 'Choose board:', choices = healthboards))
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
    
    nav_panel('Time-series',
              
              fluidRow(    
                p(linebreaks(0.5)),
                column(4, selectizeInput("ts_view", label = 'Choose view:', choices = c('Number of Paid Items',
                                                                                        'Gross Ingredient Cost (£)',
                                                                                        'Cost per item')))
                ),
              
              fluidRow(
                
                conditionalPanel(condition = "input.ts_view == 'Number of Paid Items'",
                                 fluidRow(
                                   column(10, plotOutput("items_ts", height = 600))
                                 )
                ),
                
                conditionalPanel(condition = "input.ts_view == 'Gross Ingredient Cost (£)'",
                                 fluidRow(
                                   column(10, plotOutput("gic_ts", height = 600))
                                 )
                ),
                
                conditionalPanel(condition = "input.ts_view == 'Cost per item'",
                                 fluidRow(
                                   column(10, plotOutput("cpi_ts", height = 600))
                                 )
                ),
                
                )
              
                
    ),
    
    nav_panel('Sub series',
              
              fluidRow(    
                p(linebreaks(0.5)),
                column(4, selectizeInput("ss_view", label = 'Choose view:', choices = c('Number of Paid Items',
                                                                                        'Gross Ingredient Cost (£)',
                                                                                        'Cost per item')))
              ),
              
              fluidRow(
                
                conditionalPanel(condition = "input.ss_view == 'Number of Paid Items'",
                                 fluidRow(
                                   column(10, plotOutput("items_ss"))
                                 )
                ),
                
                conditionalPanel(condition = "input.ss_view == 'Gross Ingredient Cost (£)'",
                                 fluidRow(
                                   column(10, plotOutput("gic_ss"))
                                 )
                ),
                
                conditionalPanel(condition = "input.ss_view == 'Cost per item'",
                                 fluidRow(
                                   column(10, plotOutput("cpi_ss"))
                                 )
                ),
                
              )
              
    ),
    
    nav_panel('Scatterplots',
              
              fluidRow(
                
                p(linebreaks(1)),
                column(10, plotOutput("corr_scatter", height = 600))
                
              )
              
    ),
    
    nav_panel('Lag plots',
              
              fluidRow(    
                p(linebreaks(0.5)),
                column(4, selectizeInput("lag_view", label = 'Choose view:', choices = c('Number of Paid Items',
                                                                                        'Gross Ingredient Cost (£)',
                                                                                        'Cost per item')))
              ),
              
              fluidRow(
                
                conditionalPanel(condition = "input.lag_view == 'Number of Paid Items'",
                                 fluidRow(
                                   column(10, plotOutput("items_lag", height = 600))
                                 )
                ),
                
                conditionalPanel(condition = "input.lag_view == 'Gross Ingredient Cost (£)'",
                                 fluidRow(
                                   column(10, plotOutput("gic_lag", height = 600))
                                 )
                ),
                
                conditionalPanel(condition = "input.lag_view == 'Cost per item'",
                                 fluidRow(
                                   column(10, plotOutput("cpi_lag", height = 600))
                                 )
                ),
                
              )
              
    )
    
    )
  
)
