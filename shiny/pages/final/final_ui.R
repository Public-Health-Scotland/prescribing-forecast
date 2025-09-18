####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("SARIMA forecasting"),
    p("Forecasting number of paid items using SARIMA while encorporating important factors such as monthly number of working days and items per prescription."),
    
    actionButton('notes', 'Click here for key notes'),
    p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("final_board", label = 'Choose board:', choices = healthboards_agg))
  ),
  
  fluidRow(
    layout_column_wrap(
      
      value_box( 
        title = paste0('Average absolute forecasting error over previous 12 months of observed data measuring number of paid items'), 
        value = uiOutput('items_value'),
        showcase = bs_icon("activity"),
        style = "background-color: #3F3685; color: white;",
        p(uiOutput('board'))
        
    ),
    
    value_box(
      title = paste0('Average absolute forecasting error over previous 12 months of data measuring number of paid items per working day'),
      value = uiOutput('items_wd_value'),
      showcase = bs_icon("bar-chart"),
      style = "background-color: #83BB26; color: white;",
      p(uiOutput('board_wd'))
    ),
    
    width = 1/6
    
    )
  ),
  
  fluidRow(p(linebreaks(1))),
  
  navset_pill(
    
    nav_panel('Number of Paid Items',
      
      fluidRow(
        p(linebreaks(1)),
        column(12, plotlyOutput("items_plot"))
      ),
      
      fluidRow(
        p(linebreaks(1)),
        downloadButton("downloadData_items", "Download Data")
      )
      
      ),
    
    nav_panel('Number of Paid Items per Working Day',
      
              # tabsetPanel(
              #   tabPanel("Forecast",
              #            
              #            fluidRow(    
              #              p(linebreaks(1)),
              #              column(12, plotlyOutput("items_wd_plot"))
              #            ),
              #            
              #            fluidRow(
              #              p(linebreaks(1)),
              #              downloadButton("downloadData_items_wd", "Download Data")
              #            )
              #            
              #            ), 
              #   
              #   tabPanel("Year-on-year change (%)",
              #            
              #            fluidRow(
              #              p(linebreaks(1)),
              #              column(12, plotlyOutput("change_wd"))
              #            )
              #            
              #            )
              # )
              
              fluidRow(
                column(4, selectizeInput("pi_wd_view", label = 'Choose view:', choices = c('Forecast',
                                                                                           'Year-on-year change (%)',
                                                                                           'Average national forecast error')))
              ),
              
              conditionalPanel(condition = "input.pi_wd_view == 'Forecast'", 
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("items_wd_plot")),
                                 p(linebreaks(1)),
                                 downloadButton("downloadData_items_wd", "Download Data")
                                 )),
              
              conditionalPanel(condition = "input.pi_wd_view == 'Year-on-year change (%)'", 
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("change_wd")),
                                 p(linebreaks(1)),
                                 downloadButton("downloadData_yearly_change", "Download Data")
                               )),
              
              conditionalPanel(condition = "input.pi_wd_view == 'Average national forecast error'", 
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("measure_error")),
                                 p(linebreaks(1)),
                                 downloadButton("downloadData_avg_error", "Download Data")
                               ))
              
              
    ),
    
    nav_panel('Prescription Monitoring',
              
              fluidRow(
                column(4, selectizeInput("prescription_wd_view", label = 'Choose view:', choices = c('Items per prescription',
                                                                                                     'Number of prescriptions per working day')))
              ),
              
              fluidRow(    
                p(linebreaks(1)),
                #column(12, plotlyOutput("pp_wd_plot"))
                #column(12,
                       #Cost chart
                       conditionalPanel(condition = "input.prescription_wd_view == 'Items per prescription'", 
                                        column(12, plotlyOutput('items_pp')),
                                        p(linebreaks(1)),
                                        downloadButton("downloadData_items_pp", "Download Data")
                                        ),
                
                       conditionalPanel(condition = "input.prescription_wd_view == 'Number of prescriptions per working day'", 
                                        column(12, plotlyOutput('pp_wd_plot')),
                                        p(linebreaks(1)),
                                        downloadButton("downloadData_pp_wd", "Download Data")
                                        )
                #)
              )
              
    )#,
    
    # nav_panel('Measuring forecast error',
    #           
    #           fluidRow(    
    #             p(linebreaks(1)),
    #             plotlyOutput("measure_error")
    #           )
    #           
    # )
    
    
    
    )

  # fluidRow(layout_column_wrap(
  # 
  #   value_box(
  #     title = paste0('Average forecasting error over previous 12 months of observed data'),
  #     value = uiOutput('items_wd_value'),
  #     showcase = bs_icon("piggy-bank"),
  #     style = "background-color: #674ea7; color: white;",
  #     p(uiOutput('board_wd'))
  #   ),
  # 
  #   width = 1/6
  # 
  # )),
  

  
)
