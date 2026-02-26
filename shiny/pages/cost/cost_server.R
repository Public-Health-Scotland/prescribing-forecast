
# ############## Results Server ######################

palette <- phs_colour_values

## Render output dependent on checkbox input

# output$checkbox_gic <- renderUI({
#   if (input$gic_advanced) {
#     fluidRow(
#       column(4, selectizeInput("gic_year", 
#                                label = 'Choose year:', 
#                                choices = c(2022, 2023, 2024, 2025))),
#       column(8, selectizeInput("gic_historical", 
#                                label = 'Choose number of months included in test data:', 
#                                choices = c(12, 24, 36))),
#       p(linebreaks(1))
#       )
#   } else {
#       p(linebreaks(1))
#     }
# })


## 1.1. Filter data based on board input

gic_plot_data <- reactive({
  
  data <- forecast %>%
    filter(Board == input$cost_board,
           Type == "Claim PD Paid GIC excl. BB")
  
#   if (input$gic_advanced == TRUE) {
#     data <- data %>%
#       filter(Year == input$gic_year,
#              F_Horizon == as.numeric(input$gic_historical) + 36)
#   } else if (input$gic_advanced == FALSE) {
#     data <- data %>%
#       filter(Year == 2025,
#              F_Horizon == 48) %>%
#       select(-Year, -Historical_Data, -F_Horizon, -Arima_Error)
#   }
   
 })

cpi_plot_data <- reactive({
  
  data <- forecast %>%
    filter(Board == input$cost_board,
           Type == "Cost per item")

  
  # if (input$gic_advanced == TRUE) {
  #   data <- data %>%
  #     filter(Year == input$gic_year,
  #            F_Horizon == input$gic_historical + 36)
  # } else if (input$gic_advanced == FALSE) {
  #   data <- data %>%
  #     filter(Year == 2025,
  #            F_Horizon == 48) %>%
  #     select(-Year, -Historical_Data, -F_Horizon, -Arima_Error)
  # }
  
})

## 1.2. Render plots
output$gic_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = gic_plot_data(),
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
    layout(title = paste('Forecasting monthly gross ingredient cost in', input$cost_board),
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
           yaxis = list(title = '<b>Gross Ingredient Cost (£)</b>'),
           font = list(family = 'Arial'))

  plot
  
})

output$cpi_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = cpi_plot_data(),
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
    layout(title = paste('Forecasting monthly cost per item in', input$cost_board),
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
           yaxis = list(title = '<b>Cost per item (£)</b>'),
           font = list(family = 'Arial'))
  
  plot
  
})

## Percentage change chart - number of paid items
gic_yearly_change <- reactive({
  
  forecast_gic %>%
    filter(Board == input$cost_board)
  
})


output$change_gic <- renderPlotly({
  
  plot <- plot_ly(
    data = gic_yearly_change(),
    x = ~Date,
    y = ~YoY,
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy'
  ) %>%
    layout(title = paste('Year-on-year percentage change in gross ingredient cost in', input$final_board),
           xaxis = list(title = 'Month'),
           yaxis = list(title = 'Percentage change (%)'),
           font = list(family = 'Arial'))
  
  plot
  
})

## Percentage change chart - working day
cpi_yearly_change <- reactive({
  
  forecast_cpi %>%
    filter(Board == input$cost_board)
  
})


output$change_cpi <- renderPlotly({
  
  plot <- plot_ly(
    data = cpi_yearly_change(),
    x = ~Date,
    y = ~YoY,
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy'
  ) %>%
    layout(title = paste('Year-on-year percentage change in cost per item in', input$final_board),
           xaxis = list(title = 'Month'),
           yaxis = list(title = 'Percentage change (%)'),
           font = list(family = 'Arial'))
  
  plot
  
})


## New tab - number of prescriptions per working day
# pp_wd <- reactive({
#   
#   df <- combined_data_wd %>%
#     filter(`Disp Health Board Name` == input$final_board)
#   
# })
# 
# output$pp_wd_plot <- renderPlotly({
#   
#   plot <- plot_ly(
#     data = pp_wd(),
#     x = ~`Paid Date`,
#     y = ~`No of Prescriptions per working day`,
#     name = 'Actual data',
#     type = 'scatter',
#     mode = 'lines',
#     line = list(color = phs_colours('phs-blue'))) %>%
#     # add_trace(y = ~Forecast,
#     #           name = 'Original forecast',
#     #           line = list(dash = 'dot')) %>%
#     # add_ribbons(y = ~Forecast,
#     #             ymin = ~Lower_95, ymax = ~Upper_95,
#     #             fillcolor = 'rgba(255, 0, 0, 0.2)',
#     #             line = list(color = 'rgba(255, 0, 0, 0)'),
#     #             name = '95% CI') %>%
#     # add_ribbons(y = ~Forecast,
#     #             ymin = ~Lower_80, ymax = ~Upper_80,
#     #             line = list(color = 'rgba(0,0,0,0)'),
#     #             fillcolor = 'rgba(100,100,200,0.2)',
#     #             name = '80% CI') %>%
#     # Update title and axes
#     layout(title = paste('Monthly number of prescriptions per working day in', input$final_board),
#            xaxis = list(title = 'Date',
#                         rangeslider = list(visible = TRUE,          # Enable the range slider
#                                            bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
#                                            bordercolor = phs_colours('phs-magenta'),    # Border color
#                                            borderwidth = 2),
#                         rangeselector = list(
#                           buttons = list(
#                             list(
#                               count = 6,
#                               label = "6 mo",
#                               step = "month",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "1 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 2,
#                               label = "2 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "YTD",
#                               step = "year",
#                               stepmode = "todate"),
#                             list(step = "all")))),
#            yaxis = list(title = 'Number of prescriptions per working day'),
#            font = list(family = 'Arial'))
#   
#   plot
#   
#   
# })
# 
# ## Simple chart looking at number of items per prescription
# output$items_pp <- renderPlotly({
#   
#   plot <- plot_ly(
#     data = pp_wd(),
#     x = ~`Paid Date`,
#     y = ~`Avg No of Items per prescription`,
#     name = 'Actual data',
#     type = 'scatter',
#     mode = 'lines',
#     line = list(color = phs_colours('phs-blue'))) %>%
#     # Update title and axes
#     layout(title = paste('Monthly number of paid items per prescription in', input$final_board),
#            xaxis = list(title = 'Date',
#                         rangeslider = list(visible = TRUE,          # Enable the range slider
#                                            bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
#                                            bordercolor = phs_colours('phs-magenta'),    # Border color
#                                            borderwidth = 2),
#                         rangeselector = list(
#                           buttons = list(
#                             list(
#                               count = 6,
#                               label = "6 mo",
#                               step = "month",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "1 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 2,
#                               label = "2 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "YTD",
#                               step = "year",
#                               stepmode = "todate"),
#                             list(step = "all")))),
#            yaxis = list(title = 'Average number of items per prescription'),
#            font = list(family = 'Arial'))
#   
#   plot
#   
# })
# 
# 
# ## New tab measuring forecast error
# output$measure_error <- renderPlotly({
#   
#   he_ts <- he_ts %>%
#     arrange(board, year)
#   
#   
#   plot <- ggplot(he_ts,
#          aes(x = year,
#              y = forecast_error,
#              fill = board)) +
#     geom_bar(stat = 'identity') +
#     facet_wrap(~board) +
#     geom_text(aes(label = round(forecast_error, 2))) +
#     coord_cartesian(ylim = c(0, 8)) +
#     labs(title = 'Average yearly forecasting error',
#          x = 'Year',
#          y = 'Average forecasting error (%)') +
#     theme(plot.title = element_text(hjust = 0.5, vjust = 10))
#   
#   
#   ggplotly(plot, height = 750)
#   
#   
# })
# 
# ## Cost per item tab ----
# cpi <- reactive({
#   
#   df <- cost_per_item %>%
#     filter(Board == input$final_board)
#   
# })
# 
# output$costperitem <- renderPlotly({
#   
#   plot <- plot_ly(
#     data = cpi(),
#     x = ~Date,
#     y = ~`Cost per item`,
#     name = 'Actual data',
#     type = 'scatter',
#     mode = 'lines',
#     line = list(color = phs_colours('phs-blue'))) %>%
#     # Update title and axes
#     layout(title = paste('Monthly cost per item in', input$final_board),
#            xaxis = list(title = 'Date',
#                         rangeslider = list(visible = TRUE,          # Enable the range slider
#                                            bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
#                                            bordercolor = phs_colours('phs-magenta'),    # Border color
#                                            borderwidth = 2),
#                         rangeselector = list(
#                           buttons = list(
#                             list(
#                               count = 6,
#                               label = "6 mo",
#                               step = "month",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "1 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 2,
#                               label = "2 yr",
#                               step = "year",
#                               stepmode = "backward"),
#                             list(
#                               count = 1,
#                               label = "YTD",
#                               step = "year",
#                               stepmode = "todate"),
#                             list(step = "all")))),
#            yaxis = list(title = 'Cost per item'),
#            font = list(family = 'Arial'))
#   
#   plot
#   
# })

## Notes toggle ----
# observeEvent(input$cost_notes, {
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
output$downloadData_gic <- downloadHandler(
  filename = function() {
    paste("GrossIngredientCost-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(gic_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_cpi <- downloadHandler(
  filename = function() {
    paste("CostPerItem-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(cpi_plot_data(), file, row.names = FALSE)
  }
)

# output$downloadData_yearly_change <- downloadHandler(
#   filename = function() {
#     paste("YearlyChangeWD-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(yearly_change_wd(), file, row.names = FALSE)
#   }
# )
# 
# output$downloadData_avg_error <- downloadHandler(
#   filename = function() {
#     paste("AverageError-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(he_ts, file, row.names = FALSE)
#   }
# )
# 
# output$downloadData_pp_wd <- downloadHandler(
#   filename = function() {
#     paste("PrescriptionWD-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pp_wd(), file, row.names = FALSE)
#   }
# )
# 
# output$downloadData_items_pp <- downloadHandler(
#   filename = function() {
#     paste("NumberOfPaidItemsPrescription-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pp_wd(), file, row.names = FALSE)
#   }
# )
# 
# output$downloadData_cpi <- downloadHandler(
#   filename = function() {
#     paste("CostPerItem-", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pp_wd(), file, row.names = FALSE)
#   }
# )


