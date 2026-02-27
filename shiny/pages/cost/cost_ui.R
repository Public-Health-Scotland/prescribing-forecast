####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Cost forecasting"),
    p("Forecasting gross ingredient cost and cost per item using SARIMA - refer to 'Model details' tab for more information.")#,
    
    #actionButton('cost_notes', 'Click here for key notes'),
    #p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("cost_board", label = 'Choose board:', choices = healthboards))
  ),
  
  #fluidRow(p(linebreaks(1))),
  
  navset_pill(
    
    nav_panel('Gross Ingredient Cost (£)',
              
               fluidRow(
                 p(linebreaks(0.5)),
                 column(6, selectizeInput("gic_view", label = 'Choose view:', choices = c('Forecast',
                                                                                         'Year-on-year change (%)'))),
                 conditionalPanel(condition = "input.gic_view == 'Forecast'",
                                  column(6, selectizeInput("m_q_gic_view", label = 'Aggregation level:', choices = c('Monthly',
                                                                                                                     'Quarterly'))))
               ),


              conditionalPanel(condition = "input.gic_view == 'Forecast'",
                               fluidRow(
                                 conditionalPanel(condition = "input.m_q_gic_view == 'Monthly'",
                                                  column(10, plotlyOutput("gic_plot"))
                                 ),
                                 conditionalPanel(condition = "input.m_q_gic_view == 'Quarterly'",
                                                  column(10, plotlyOutput("gic_quarterly_plot"))
                                 )
                               ),
                               fluidRow(
                                 conditionalPanel(condition = "input.m_q_gic_view == 'Monthly'",
                                                  downloadButton("downloadData_gic", "Download Data", style = "width:200px;")),
                                 
                                 conditionalPanel(condition = "input.m_q_gic_view == 'Quarterly'",
                                                  downloadButton("downloadData_gic_quarterly", "Download Data", style = "width:200px;"))
                               )
                               ),
              
              conditionalPanel(condition = "input.gic_view == 'Year-on-year change (%)'",
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(10, plotlyOutput("change_gic")),
                                 p(linebreaks(1))#,
                                 #downloadButton("downloadData_yearly_change", "Download Data")
                               )
                               )
      
      ),
    
    nav_panel('Cost per item',
      
              fluidRow(
                p(linebreaks(0.5)),
                column(4, selectizeInput("cpi_view", label = 'Choose view:', choices = c('Forecast',
                                                                                         'Year-on-year change (%)')))
              ),
              
              conditionalPanel(condition = "input.cpi_view == 'Forecast'",
                               fluidRow(
                                 column(10, plotlyOutput("cpi_plot"))
                               ),
                               fluidRow(
                                 downloadButton("downloadData_cpi", "Download Data", style = "width:200px;")
                                 )
                               ),
              
              conditionalPanel(condition = "input.cpi_view == 'Year-on-year change (%)'",
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(10, plotlyOutput("change_cpi")),
                                 p(linebreaks(1))#,
                                 #downloadButton("downloadData_yearly_change", "Download Data")
                               )
              )
              
              
    )#,
    
    )
  
)
