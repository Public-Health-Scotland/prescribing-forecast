####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Cost forecasting"),
    p("Forecasting gross ingredient cost and cost per item using SARIMA - refer to 'Model details' tab for more information."),
    
    #actionButton('cost_notes', 'Click here for key notes'),
    p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    column(4, selectizeInput("cost_board", label = 'Choose board:', choices = healthboards))
  ),
  
  #fluidRow(p(linebreaks(1))),
  
  navset_pill(
    
    nav_panel('Gross Ingredient Cost (£)',
              
               fluidRow(
                 p(linebreaks(0.5)),
                 column(4, selectizeInput("gic_view", label = 'Choose view:', choices = c('Forecast',
                                                                                         'Year-on-year change (%)')))#,
                 #column(4, checkboxInput("gic_advanced", 'Tick for advanced controls', value = FALSE))
               ),

              #uiOutput("checkbox_gic"),
              
              conditionalPanel(condition = "input.gic_view == 'Forecast'",
                               fluidRow(
                                 column(12, plotlyOutput("gic_plot")),
                                 p(linebreaks(1)),
                                 downloadButton("downloadData_gic", "Download Data")
                               )),
              
              conditionalPanel(condition = "input.gic_view == 'Year-on-year change (%)'",
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("change_gic")),
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
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("cpi_plot")),
                                 p(linebreaks(1)),
                                 downloadButton("downloadData_cpi", "Download Data")
                               )),
              
              conditionalPanel(condition = "input.cpi_view == 'Year-on-year change (%)'",
                               fluidRow(
                                 p(linebreaks(1)),
                                 column(12, plotlyOutput("change_cpi")),
                                 p(linebreaks(1))#,
                                 #downloadButton("downloadData_yearly_change", "Download Data")
                               )
              )
              
              
    )#,
    
    )
  
)
