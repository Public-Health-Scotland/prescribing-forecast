####################### Clinical Quality Indicators Page #######################


tagList(
  ## Heading and introductory text
  fluidRow(
    h1("Healthboard SARIMA forecasting"),
    p("Forecasting number of paid items using SARIMA")
  ),
  
  fluidRow(
    # column(2, selectInput('year', label = 'Choose FY:', choices = financial_years))
    column(2, selectInput('board', label = 'Choose board:', choices = healthboards))
  ),
  
  fluidRow(
    column(12, plotlyOutput('forecast_plot', width = '80%', height = '600px'))
  )
  

)



