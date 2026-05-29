# ############## Forecast Performance Server ######################

## 1.1. Wrangle table for output
forecast_performance_wide <- forecast_performance %>%
    select(board, type, forecast_error) %>%
    pivot_wider(names_from = type, 
                values_from = forecast_error)

output$performance_table <- DT::renderDT({
  
  colour_vector <- ifelse(
    forecast_performance_wide$`Claim PD Number of Paid Items` > forecast_performance_wide$`Paid Items per Working Day`,
    'lightgreen',
    'lightcoral'
  )
  
  DT::datatable(forecast_performance_wide) %>%
    DT::formatStyle(
      'Claim PD Number of Paid Items',
      backgroundColor = colour_vector
    )
  
})





