# ############## Forecast Performance Server ######################

## 1.1. Wrangle table for output
accuracy_output <- reactive({
  
  if (input$performance_hb == "All boards") {
    
    accuracy %>%
      filter(`Prescribing Health Board` %in% performance_hbs, # Filter for vector as Scotland is included in table
             Measure %in% input$performance_measure,
             `Run Number` %in% input$performance_run) 
    
  } else {
    
    accuracy %>%
      filter(`Prescribing Health Board` %in% input$performance_hb,
             Measure %in% input$performance_measure,
             `Run Number` %in% input$performance_run) 
    
  }
  
})

output$performance_table <- DT::renderDT({
  
  make_table(accuracy_output())
  
})





