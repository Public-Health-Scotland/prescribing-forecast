####################### Results Comparison Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Healthboard SARIMA forecasting"),
    p("Forecasting number of paid items using SARIMA")  
    ),
  
  fluidRow(
    column(2, selectInput('results_board', label = 'Choose board:', choices = healthboards, selected = healthboards[1]))
  ),
  
  fluidRow(
    
    column(12, DT::dataTableOutput('results_table', width = '80%', height = '600px'))
    
   ),
  
  fluidRow(),
  
  fluidRow(
    
    column(2, sliderInput('ar_value', 'Select the auto-regressive (AR) value:', min = 0, max = 9, value = 1)),
    column(2, sliderInput('d_value', 'Select the differencing (d) value:', min = 0, max = 1, value = 1, step = 1)),
    column(2, sliderInput('ma_value', 'Select the moving average (MA) value:', min = 0, max = 9, value = 1))
    
  ),
  
  fluidRow(
    column(12, plotOutput('plot', width = '80%', height = '600px'))
  )

  



  # fluidRow(
  #   column(12, plotlyOutput('forecast_plot', width = '80%', height = '600px'))
  # )

  
)
