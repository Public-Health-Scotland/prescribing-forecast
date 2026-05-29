
# ############## Results Server ######################

palette <- phs_colour_values

## New tab - number of prescriptions per working day
pp_wd <- reactive({
  
  df <- combined_data_wd %>%
    filter(`Presc Health Board Name` == input$monitoring_board)
  
})

df <- combined_data_wd %>%
  filter(`Paid Date` == '2025-06-30') 

plot <- plot_ly(
  data = df,
  x = ~`Presc Health Board Name`,
  y = ~`Avg No of Items per prescription`,
  color = ~`Presc Health Board Name`,
  colors = setNames(palette, levels(df$`Presc Health Board Name`)),
  type = 'bar') %>%
  layout(barmode = 'group') 

plot

output$pp_wd_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = pp_wd(),
    x = ~`Paid Date`,
    y = ~`No of Prescriptions per working day`,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines',
    line = list(color = phs_colours('phs-blue'))) %>%
    # add_trace(y = ~Forecast,
    #           name = 'Original forecast',
    #           line = list(dash = 'dot')) %>%
    # add_ribbons(y = ~Forecast,
    #             ymin = ~Lower_95, ymax = ~Upper_95,
    #             fillcolor = 'rgba(255, 0, 0, 0.2)',
    #             line = list(color = 'rgba(255, 0, 0, 0)'),
    #             name = '95% CI') %>%
    # add_ribbons(y = ~Forecast,
    #             ymin = ~Lower_80, ymax = ~Upper_80,
    #             line = list(color = 'rgba(0,0,0,0)'),
    #             fillcolor = 'rgba(100,100,200,0.2)',
    #             name = '80% CI') %>%
    # Update title and axes
    layout(title = paste('Monthly number of prescriptions per working day in', input$monitoring_board),
           xaxis = list(title = 'Date',
                        rangeslider = list(visible = TRUE,          # Enable the range slider
                                           bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                           bordercolor = phs_colours('phs-magenta'),    # Border color
                                           borderwidth = 2)),
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
                        #     list(step = "all")))),
           yaxis = list(title = 'Number of prescriptions per working day'),
           font = list(family = 'Arial'))
  
  plot
  
  
})

## Simple chart looking at number of items per prescription
output$items_pp <- renderPlotly({
  
  plot <- plot_ly(
    data = pp_wd(),
    x = ~`Paid Date`,
    y = ~`Avg No of Items per prescription`,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines',
    line = list(color = phs_colours('phs-blue'))) %>%
    # Update title and axes
    layout(title = paste('Monthly number of paid items per prescription in', input$monitoring_board),
           xaxis = list(title = 'Date',
                        rangeslider = list(visible = TRUE,          # Enable the range slider
                                           bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                           bordercolor = phs_colours('phs-magenta'),    # Border color
                                           borderwidth = 2)),
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
                        #     list(step = "all")))),
           yaxis = list(title = 'Average number of items per prescription'),
           font = list(family = 'Arial'))
  
  plot
  
})

## Export data syntax and button ----
### Download handler
output$downloadData_pp_wd <- downloadHandler(
  filename = function() {
    paste("PrescriptionWD-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pp_wd(), file, row.names = FALSE)
  }
)

output$downloadData_items_pp <- downloadHandler(
  filename = function() {
    paste("NumberOfPaidItemsPrescription-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pp_wd(), file, row.names = FALSE)
  }
)



