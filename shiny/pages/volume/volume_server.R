
# ############## Results Server ######################

palette <- phs_colour_values

## 1.1. Filter data based on board input

items_plot_data <- filter_data(forecast_items,
                               reactive(input$items_board))

items_quarterly_plot_data <- filter_data(forecast_items_quarterly,
                                         reactive(input$items_board))


items_wd_plot_data <- filter_data(forecast_items_wd,
                                  reactive(input$items_board))

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
output$items_plot <- make_chart(items_plot_data,
                                "Number of Paid Items",
                                "monthly",
                                reactive(input$items_board))

output$items_quarterly_plot <- make_chart(items_quarterly_plot_data,
                                          "Number of Paid Items",
                                          "quarterly",
                                          reactive(input$items_board))
  
  
output$items_wd_plot <- make_chart(items_wd_plot_data,
                                   "Number of Paid Items",
                                   "quarterly",
                                   reactive(input$items_board))

## Percentage change chart - number of paid items
yearly_change <- reactive({
  
  forecast_items %>%
    filter(Board == input$final_board)
  
})


output$change_items <- make_yoy_chart(items_plot_data,
                                      "Number of Paid Items",
                                      reactive(input$items_board))
  
## Percentage change chart - working day
yearly_change_wd <- reactive({
  
  forecast_items_wd %>%
    filter(Board == input$final_board)
  
})


output$change_wd <- make_yoy_chart(items_wd_plot_data,
                                   "Number of Paid Items",
                                   reactive(input$items_board))

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

## Export data syntax and button ----
### Download handler
output$downloadData_items <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-MonthlyPaidItems-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(items_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_items_quarterly <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-QuarterlyPaidItems-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(items_quarterly_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_items_wd <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-PaidItemsWD-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(items_wd_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_yearly_change <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-YearlyChangeWD-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(yearly_change_wd(), file, row.names = FALSE)
  }
)

output$downloadData_avg_error <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-AverageError-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(he_ts, file, row.names = FALSE)
  }
)

output$downloadData_cpi <- downloadHandler(
  filename = function() {
    paste(input$final_board, "-CostPerItem-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pp_wd(), file, row.names = FALSE)
  }
)


