
# ############## Results Server ######################

## 1.1. Filter data based on financial year input
# filtered_data <- reactive({
#   
#   data <- forecasted_data %>%
#     filter(FY == input$year) %>%
#     select(Board, Date, FY, Phasings, second_forecast) %>%
#     pivot_longer(cols = c(Phasings, second_forecast),
#                  names_to = "series",
#                  values_to = "value")
#   
# })

results <- reactive({
  
  data <- results_comparison %>%
    filter(board == input$results_board)
  
})

output$results_table <- DT::renderDataTable({
  
  make_table(results())
  
})

## 1.2. Create plot
# output$accuracy_plot <- renderPlotly({
#   
#   plot <- ggplot(items(), aes(x = Date, y = value, color = series)) +
#     geom_line() +
#     facet_wrap(~Board)
#   
#   ggplotly(plot)
#   
#   
# })

# time_series <- reactive({
#   
#   df <- data %>%
#     filter(Board == input$results_board) %>%
#     select(Date, `Claim PD Number of Paid Items`) %>%
#     filter(Date > '2005-12-31' & Date < '2024-01-01')
#   
#   y <- ts(df$`Claim PD Number of Paid Items`,
#           start = 2006,
#           frequency = 12)
#   
# })
# 
# forecast_f <- reactive({
#   
#   fc <- Arima(time_series(), order = c(input$ar_value, input$d_value, input$ma_value), seasonal = c(1,0,1))
#   
#   forecast(fc, h = 24)
#   
# })
# 
# 
# output$forecast_plot <- renderPlotly({
# 
#   p <- plot_ly() %>%
#     # Historical data
#     add_lines(
#       x = time(time_series()),
#       y = as.numeric(time_series()),
#       name = "Observed"
#     ) %>%
#     # Forecasted mean
#     add_lines(
#       x = time(forecast_f()$mean),
#       y = as.numeric(forecast_f()$mean),
#       name = "Forecast"
#     ) %>%
#     # Confidence interval (upper + lower ribbon)
#     add_ribbons(
#       x = time(forecast_f()$mean),
#       ymin = as.numeric(forecast_f()$lower[,2]),  # 95% lower
#       ymax = as.numeric(forecast_f()$upper[,2]),  # 95% upper
#       name = "95% CI",
#       line = list(color = "transparent"),
#       fillcolor = "rgba(0,100,80,0.2)"
#     )
  

time_series <- reactive({
  
  df <- data %>%
    filter(Board == input$results_board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(Date > '2005-12-31' & Date < '2024-01-01')
  
  time_series <- list(
    ts = ts(df$`Claim PD Number of Paid Items`,
            start = c(2006, 1), frequency = 12),
    dates = df$Date
  )
  
})

forecast_f <- reactive({
  
  ts_data <- time_series()$ts
  
  fc <- Arima(ts_data, 
              order = c(input$ar_value, input$d_value, input$ma_value), 
              seasonal = c(1,0,1))
  
  forecast_f <- forecast(fc, h = 24)
  
})

plot <- reactive({
  
  ts_data <- time_series()
  fc <- forecast_f()
  
  # Forecast horizon dates
  future_dates <- seq(max(ts_data$dates) %m+% months(1),
                      by = "month", length.out = 24)
  
  p <- plot_ly() %>%
    # Historical data
    add_lines(
      x = ts_data$dates,
      y = as.numeric(ts_data$ts),
      name = "Observed"
    ) %>%
    # Forecasted mean
    add_lines(
      x = future_dates,
      y = as.numeric(fc$mean),
      name = "Forecast"
    ) %>%
    # Confidence interval
    add_ribbons(
      x = future_dates,
      ymin = as.numeric(fc$lower[,2]),
      ymax = as.numeric(fc$upper[,2]),
      name = "95% CI",
      line = list(color = "transparent"),
      fillcolor = "rgba(0,100,80,0.2)"
    ) %>%
    # Confidence interval
    add_ribbons(
      x = future_dates,
      ymin = as.numeric(fc$lower[,1]),
      ymax = as.numeric(fc$upper[,1]),
      name = "80% CI",
      line = list(color = "transparent"),
      fillcolor = "rgba(255,0,0,0.3)"
    )
  
  p
  
})


output$forecast_plot <- renderPlotly({
  
  plot()
  
})

  # p <- plot_ly(
  #   data = time_series(),
  #   x = time(time_series()),
  #   y = as.numeric(time_series()),
  #   name = 'Historical',
  #   type = 'scatter',
  #   mode = 'lines') %>%
  #     # Forecasted mean
  #     add_lines(
  #       x = time(forecast_f()$mean),
  #       y = as.numeric(forecast_f()$mean),
  #       name = "Forecast"
  #     ) %>%
  #     # Confidence interval (upper + lower ribbon)
  #     add_ribbons(
  #       x = time(forecast_f()$mean),
  #       ymin = as.numeric(forecast_f()$lower[,2]),  # 95% lower
  #       ymax = as.numeric(forecast_f()$upper[,2]),  # 95% upper
  #       name = "95% CI",
  #       line = list(color = "transparent"),
  #       fillcolor = "rgba(0,100,80,0.2)"
  #     )
    
  
#   p
# 
# })

output$plot <- renderPlot({
  
  forecast_f() %>% autoplot()
  
})