
# ############## Results Server ######################

palette <- phs_colour_values

## 1.1. Filter data based on board input

items_plot_data <- reactive({
  
  data <- forecast_items %>%
    filter(Board == input$final_board)
  
})

items_wd_plot_data <- reactive({
  
  data <- forecast_items_wd %>%
    filter(Board == input$final_board)
  
})

## 1.2. Create highlight statistics
highlight_data <- reactive({
  
  df <- forecast_performance %>%
    filter(board == input$final_board)
  
}) 

output$board <- renderText({
  
  paste0(input$final_board)
  
})

output$board_wd <- renderText({
  
  paste0(input$final_board)
  
})

output$items_value <- renderText({
  
  df <- highlight_data() %>%
    filter(type == 'Claim PD Number of Paid Items')
  
  paste0(round(df$forecast_error, 3), '%')
  
})

output$items_wd_value <- renderText({
  
  df <- highlight_data() %>%
    filter(type == 'Paid Items per Working Day')
  
  paste0(round(df$forecast_error, 3), '%')
  
})

## 1.3. Render plots
output$items_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = items_plot_data(),
    x = ~Date,
    y = ~Measure,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines') %>%
    add_trace(y = ~Forecast,
              name = 'Forecast',
              line = list(dash = 'dot')) %>%
    add_ribbons(#data = items_plot_data(),
      #x = ~Date,
      y = ~Forecast,
      ymin = ~Lower_95, ymax = ~Upper_95,
      fillcolor = 'rgba(255, 0, 0, 0.2)',
      line = list(color = 'rgba(255, 0, 0, 0)'),
      name = '95% CI') %>%
    add_ribbons(#data = items_plot_data(),
      #x = ~Date,
      y = ~Forecast,
      ymin = ~Lower_80, ymax = ~Upper_80,
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(100,100,200,0.2)',
      name = '80% CI') %>%
    # Update title and axes
    layout(title = paste('Forecasting monthly number of paid items in', input$final_board),
           xaxis = list(title = '<b>Date</b>',
                        rangeslider = list(visible = TRUE,          # Enable the range slider
                                           bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                           bordercolor = phs_colours('phs-magenta'),    # Border color
                                           borderwidth = 2)#, 
                        # rangeselector = list(
                        #   buttons = list(
                        #     list(
                        #       count = 6,
                        #       label = "6 mo",
                        #       step = "month",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 1,
                        #       label = "1 yr",
                        #       step = "year",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 2,
                        #       label = "2 yr",
                        #       step = "year",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 1,
                        #       label = "YTD",
                        #       step = "year",
                        #       stepmode = "todate"),
                        #     list(step = "all")))
                        ),
           yaxis = list(title = '<b>Number of Paid Items</b>'),
           font = list(family = 'Arial'))

  plot
  
})

output$items_wd_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = items_wd_plot_data(),
    x = ~Date,
    y = ~Measure,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines'#,
    #line = list(color = phs_colours('phs-blue'))
    ) %>%
    add_trace(y = ~Forecast,
              name = 'Forecast',
              line = list(dash = 'dot')) %>%
    add_ribbons(y = ~Forecast,
                ymin = ~Lower_95, ymax = ~Upper_95,
                fillcolor = 'rgba(255, 0, 0, 0.2)',
                line = list(color = 'rgba(255, 0, 0, 0)'),
                name = '95% CI') %>%
    add_ribbons(y = ~Forecast,
                ymin = ~Lower_80, ymax = ~Upper_80,
                line = list(color = 'rgba(0,0,0,0)'),
                fillcolor = 'rgba(100,100,200,0.2)',
                name = '80% CI') %>%
    # Update title and axes
    layout(title = paste('Forecasting monthly number of paid items per working day in', input$final_board),
           xaxis = list(title = '<b>Date</b>',
                        rangeslider = list(visible = TRUE,          # Enable the range slider
                                           bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                           bordercolor = phs_colours('phs-magenta'),    # Border color
                                           borderwidth = 2)#,
                        # rangeselector = list(
                        #   buttons = list(
                        #     list(
                        #       count = 6,
                        #       label = "6 mo",
                        #       step = "month",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 1,
                        #       label = "1 yr",
                        #       step = "year",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 2,
                        #       label = "2 yr",
                        #       step = "year",
                        #       stepmode = "backward"),
                        #     list(
                        #       count = 1,
                        #       label = "YTD",
                        #       step = "year",
                        #       stepmode = "todate"),
                        #     list(step = "all")))
                        ),
           yaxis = list(title = '<b>Number of paid items per working day</b>'),
           font = list(family = 'Arial'))
  
  plot
  
})

## Percentage change chart - number of paid items
yearly_change <- reactive({
  
  forecast_items %>%
    filter(Board == input$final_board)
  
})


output$change_items <- renderPlotly({
  
  plot <- plot_ly(
    data = yearly_change(),
    x = ~Date,
    y = ~YoY,
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy'
  ) %>%
    layout(title = paste('Year-on-year percentage change in number of paid items in', input$final_board),
           xaxis = list(title = 'Month'),
           yaxis = list(title = 'Percentage change (%)'),
           font = list(family = 'Arial'))
  
  plot
  
})

## Percentage change chart - working day
yearly_change_wd <- reactive({
  
  forecast_items_wd %>%
    filter(Board == input$final_board)
  
})


output$change_wd <- renderPlotly({
  
  plot <- plot_ly(
    data = yearly_change_wd(),
    x = ~Date,
    y = ~YoY,
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy'
  ) %>%
    layout(title = paste('Year-on-year percentage change in number of paid items per working day in', input$final_board),
           xaxis = list(title = 'Month'),
           yaxis = list(title = 'Percentage change (%)'),
           font = list(family = 'Arial'))
  
  plot
  
})

## New tab measuring forecast error
output$measure_error <- renderPlotly({
  
  he_ts <- he_ts %>%
    arrange(board, year)
  
  
  plot <- ggplot(he_ts,
         aes(x = year,
             y = forecast_error,
             fill = board)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~board) +
    geom_text(aes(label = round(forecast_error, 2))) +
    coord_cartesian(ylim = c(0, 8)) +
    labs(title = 'Average yearly forecasting error',
         x = 'Year',
         y = 'Average forecasting error (%)') +
    theme(plot.title = element_text(hjust = 0.5, vjust = 10))
  
  
  ggplotly(plot, height = 750)
  
  
})

## Notes toggle ----
# observeEvent(input$notes, {
#   
#   showModal(modalDialog(
#     
#     HTML(paste("In this iteration of the forecasting model, the SARIMA (Seasonal Auto-Regressive Integrated Moving Average) statistical model has been used to generate the forecasts seen within this dashboard.",
#                "<br>The model can be defined in the format below:</br>",
#                "<p style='text-align: center;'>SARIMA(p, d, q)(1, 0, 1)[12]</p>",
#                "Each SARIMA model has two parts to it - a seasonal part (the second set of brackets) and a non-seasonal (first set of brackets) part. Values in the non-seasonal part are denoted as p, d and q, while the seasonal part is the same but denoted in upper-case. 'p' deals with the auto-regressive part of the mode, 'q' quantifies the number of times the data is differenced to achieve stationarity, and 'p' is the moving average component of the model. The value in the square brackets defines the time series period, which in this case is monthly.<br>", 
#                "Following model testing on the time series data used, the seasonal part was able to be quantified as (1, 0, 1) while the non-seasonal part was particularly tricky to define. To deal with this problem, the model first runs with the seasonal and periodic parts of the model pre-defined while looping through each possible combination of the three non-seasonal parameters (values of p and d between 0 and 9, q between 0 and 1).<br>",
#                "Say for example, the model is required to forecast twelve months from June 2025 for NHS Ayrshire & Arran. The model will use time series data until June 2024 and generate forecasts for the following twenty-four months. This is so predicted values for the first twelve months can be compared to real, observed data. This comparison will calculate an absolute average error between the two sets of data, and at the end of the looping process take the model with the lowest calculated error as the best fit. Computing the model in this way means each board will have a model that best fits their data at the time of running (based on the time series boundaries). Scotland figures can then be calculated once each board's model has been generated, by aggregating the forecasted values and its confidence intervals - ensuring the variability in each board is taken into account.",
#                
#          sep="<br/>"),
#     collapse = "<br><br>"),
#     
#     easyClose = TRUE,
#     size = 'l'
#     
#   ))
#   
# }) 


## Export data syntax and button ----
### Download handler
output$downloadData_items <- downloadHandler(
  filename = function() {
    paste("NumberOfPaidItems-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(items_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_items_wd <- downloadHandler(
  filename = function() {
    paste("NumberOfPaidItemsWD-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(items_wd_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_yearly_change <- downloadHandler(
  filename = function() {
    paste("YearlyChangeWD-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(yearly_change_wd(), file, row.names = FALSE)
  }
)

output$downloadData_avg_error <- downloadHandler(
  filename = function() {
    paste("AverageError-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(he_ts, file, row.names = FALSE)
  }
)

output$downloadData_cpi <- downloadHandler(
  filename = function() {
    paste("CostPerItem-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pp_wd(), file, row.names = FALSE)
  }
)


