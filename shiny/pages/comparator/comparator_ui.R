####################### Comparator Tab Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Forecast comparison between two boards"),
    p("Use the dropdowns below to compare the forecasts of two different boards for a forecast and aggregation level of your choice."),
    tags$b("Quarterly aggregation is not available for the cost per item forecast."),
    
    #actionButton('notes', 'Click here for key notes'),
    p(linebreaks(0.5))

    
  ),
  
  fluidRow(
    
    column(12,
           style = "padding-right:50px;", # adds space on right hand side
           
           fluidRow(
             column(3, selectizeInput("first_board", label = 'Select first board:', choices = healthboards, selected = 'SCOTLAND')), 
             
             column(3, selectizeInput("second_board", label = 'Select second board:', choices = healthboards, selected = 'NHS AYRSHIRE & ARRAN')),
             
             column(3, selectizeInput("comparator_measure", label = 'Measure:', choices = c('Number of Paid Items',
                                                                                            'Gross Ingredient Cost',
                                                                                            'Cost per item'))),
             
             column(3, selectizeInput("comparator_agg", label = 'Aggregation:', choices = NULL)) # Choices are different dependent on "comparator_measure" - updated in comparator_server.R
           )
           
    )
    
  ),
  

  fluidRow(
    
    column(12,
           style = "padding-right:50px;", # adds space on right hand side
           
    
      # Chart for first board
      fluidRow(
        column(6,
              
               conditionalPanel(
                 condition = "input.comparator_measure == 'Number of Paid Items' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("items_plot_first_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Number of Paid Items' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("items_quarterly_plot_first_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Gross Ingredient Cost' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("gic_plot_first_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Gross Ingredient Cost' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("gic_quarterly_plot_first_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Cost per item' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("cpi_plot_first_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Cost per item' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("cpi_quarterly_plot_first_board")
               )
               
               ),
        
        column(6,
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Number of Paid Items' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("items_plot_second_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Number of Paid Items' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("items_quarterly_plot_second_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Gross Ingredient Cost' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("gic_plot_second_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Gross Ingredient Cost' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("gic_quarterly_plot_second_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Cost per item' && input.comparator_agg == 'Monthly'",
                 plotlyOutput("cpi_plot_second_board")
               ),
               
               conditionalPanel(
                 condition = "input.comparator_measure == 'Cost per item' && input.comparator_agg == 'Quarterly'",
                 plotlyOutput("cpi_quarterly_plot_second_board")
               )
             
        ),
        
        p(linebreaks(0.5)),
        tags$b("If you wish to download the forecast data, please go to the relevant tab.")
        
      )
    )
  )

)