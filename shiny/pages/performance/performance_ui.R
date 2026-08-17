####################### Forecast Performance Page #######################

tagList(
  ## Heading and introductory text
  fluidRow(
    
    h1("Forecast performance comparison"),
    p("Comparison between forecast performance of the number of paid items and the number of paid items per working day")
    
  ),
  
  fluidRow(
    column(12,
           style = "padding-right:50px;", # adds space on right hand side
           
           fluidRow(
             
             column(3, selectizeInput("performance_hb", "Choose board:", 
                                      choices = performance_hbs, 
                                      selected = performance_hbs[1])),
             
             column(3, selectizeInput("performance_measure", "Choose measure:", 
                                      choices = measures, selected = measures[1])),
             
             column(3, selectizeInput("performance_run", "Choose run number:", 
                                      choices = run_numbers, multiple = TRUE,
                                      selected = run_numbers[1]))
           )
           
           )
           ),
  
  fluidRow(linebreaks(1)),
  
  fluidRow(
    column(12,
           style = "padding-right:50px;", # adds space on right hand side
           
           dataTableOutput("performance_table")
           )
  )
  
)