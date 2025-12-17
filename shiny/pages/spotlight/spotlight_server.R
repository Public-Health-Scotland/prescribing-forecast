###################### Spotlight Server ######################

palette <- phs_colour_values

# 1. Wrangle data ----
performance <- sarima_v2.1_performance %>%
  filter(year == 2025,
         historical_data == 12)

# 1. KPIs ----
output$best_accuracy_items_board <- renderText({
  
  df <- performance %>%
    filter(suffix == "items") %>%
    filter(forecast_error == min(forecast_error))
  
  df$board
  
})

output$best_accuracy_items_number <- renderText({
  
  df <- performance %>%
    filter(suffix == "items") %>%
    filter(forecast_error == min(forecast_error))
  
  paste0(round(df$forecast_error, 3), '%')
  
})

output$best_accuracy_gic_board <- renderText({
  
  df <- performance %>%
    filter(suffix == "gic") %>%
    filter(forecast_error == min(forecast_error))
  
  df$board
  
})

output$best_accuracy_gic_number <- renderText({
  
  df <- performance %>%
    filter(suffix == "gic") %>%
    filter(forecast_error == min(forecast_error))
  
  paste0(round(df$forecast_error, 3), '%')
  
})

output$best_accuracy_cpi_board <- renderText({
  
  df <- performance %>%
    filter(suffix == "cpi") %>%
    filter(forecast_error == min(forecast_error))
  
  df$board
  
})

output$best_accuracy_cpi_number <- renderText({
  
  df <- performance %>%
    filter(suffix == "cpi") %>%
    filter(forecast_error == min(forecast_error))
  
  paste0(round(df$forecast_error, 3), '%')
  
})

## 1.1. Number of Paid Items ----
