####################### Forecast Performance Page #######################

tagList(
  ## Heading and introductory text
  fluidRow(
    
    h1("Forecast performance comparison"),
    p("Comparison between forecast performance of the number of paid items and the number of paid items per working day")
    
  ),
  
  fluidRow(
    column(12, DT::DTOutput('performance_table', width = '80%', height = '600px'))
  )
  
)