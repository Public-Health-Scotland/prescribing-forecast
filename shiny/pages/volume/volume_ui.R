####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Volume forecasting"),
    p("Forecasting number of paid items using SARIMA - refer to 'Model details' tab for more information.")#,
    
    #actionButton('notes', 'Click here for key notes'),
    #p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("items_board", label = 'Choose board:', choices = healthboards, selected = 'SCOTLAND'))
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
    
    nav_panel('Number of Paid Items',
              
              fluidRow(
                p(linebreaks(0.5)),
                column(6, selectizeInput("pi_view", label = 'Choose view:', choices = c('Forecast',
                                                                                        'Year-on-year change (%)')),
                       ),
                conditionalPanel(condition = "input.pi_view == 'Forecast'",
                                 column(6, selectizeInput("m_q_view", label = 'Aggregation level:', choices = c('Monthly',
                                                                                                                'Quarterly'))))
              ),
              
              conditionalPanel(condition = "input.pi_view == 'Forecast'", 
                               fluidRow(
                                 conditionalPanel(condition = "input.m_q_view == 'Monthly'",
                                                  column(10, plotlyOutput("items_plot"))
                                 ),
                                 conditionalPanel(condition = "input.m_q_view == 'Quarterly'",
                                                  column(10, plotlyOutput("items_quarterly_plot"))
                                 )
                               ),
                               fluidRow(
                                 conditionalPanel(condition = "input.m_q_view == 'Monthly'",
                                                  downloadButton("downloadData_items", "Download Data", style = "width:200px;")),

                                 conditionalPanel(condition = "input.m_q_view == 'Quarterly'",
                                                  downloadButton("downloadData_items_quarterly", "Download Data", style = "width:200px;"))
                                 )
                                
                              
                               ),
              
              conditionalPanel(condition = "input.pi_view == 'Year-on-year change (%)'", 
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(10, plotlyOutput("change_items")),
                                 p(linebreaks(1))#,
                                 #downloadButton("downloadData_yearly_change", "Download Data")
                               ))
      
      )#,
    
    # nav_panel('Number of Paid Items per Working Day',
    #   
    #           # tabsetPanel(
    #           #   tabPanel("Forecast",
    #           #            
    #           #            fluidRow(    
    #           #              p(linebreaks(1)),
    #           #              column(12, plotlyOutput("items_wd_plot"))
    #           #            ),
    #           #            
    #           #            fluidRow(
    #           #              p(linebreaks(1)),
    #           #              downloadButton("downloadData_items_wd", "Download Data")
    #           #            )
    #           #            
    #           #            ), 
    #           #   
    #           #   tabPanel("Year-on-year change (%)",
    #           #            
    #           #            fluidRow(
    #           #              p(linebreaks(1)),
    #           #              column(12, plotlyOutput("change_wd"))
    #           #            )
    #           #            
    #           #            )
    #           # )
    #           
    #           fluidRow(
    #             p(linebreaks(0.5)),
    #             column(4, selectizeInput("pi_wd_view", label = 'Choose view:', choices = c('Forecast',
    #                                                                                        'Year-on-year change (%)',
    #                                                                                        'Average national forecast error')))
    #           ),
    #           
    #           conditionalPanel(condition = "input.pi_wd_view == 'Forecast'", 
    #                            fluidRow(
    #                              p(linebreaks(1)),
    #                              column(12, plotlyOutput("items_wd_plot")),
    #                              p(linebreaks(1)),
    #                              downloadButton("downloadData_items_wd", "Download Data")
    #                              )),
    #           
    #           conditionalPanel(condition = "input.pi_wd_view == 'Year-on-year change (%)'", 
    #                            fluidRow(
    #                              p(linebreaks(1)),
    #                              column(12, plotlyOutput("change_wd")),
    #                              p(linebreaks(1)),
    #                              downloadButton("downloadData_yearly_change", "Download Data")
    #                            )),
    #           
    #           conditionalPanel(condition = "input.pi_wd_view == 'Average national forecast error'", 
    #                            fluidRow(
    #                              p(linebreaks(1)),
    #                              column(12, plotlyOutput("measure_error")),
    #                              p(linebreaks(1)),
    #                              downloadButton("downloadData_avg_error", "Download Data")
    #                            ))
    #           
    #           
    # )
    
    )
  
)
