
# ############## Results Server ######################

palette <- phs_colour_values

## 1.1. Filter data based on board input

gic_plot_data <- filter_data(forecast_gic,
                             reactive(input$cost_board))

gic_quarterly_plot_data <- filter_data(forecast_gic_quarterly,
                                       reactive(input$cost_board))

cpi_plot_data <- filter_data(forecast_cpi,
                             reactive(input$cost_board))

phasings_plot_data <- filter_data(forecast_phasings,
                                  reactive(input$cost_board))

## 1.2. Render plots
output$gic_plot <- make_chart(gic_plot_data,
                              "Gross Ingredient Cost (£)",
                              "monthly",
                              reactive(input$cost_board))

output$gic_quarterly_plot <- make_chart(gic_quarterly_plot_data,
                                        "Gross Ingredient Cost (£)",
                                        "quarterly",
                                        reactive(input$cost_board))

output$cpi_plot <- make_chart(cpi_plot_data,
                              "Cost per item",
                              "monthly",
                              reactive(input$cost_board))

# output$phasings_plot <- make_chart(phasings_plot_data,
#                                    "Phasings",
#                                    "monthly",
#                                    reactive(input$cost_board))

output$phasings_plot <- renderPlotly({
  
  plot <- plot_ly(
    phasings_plot_data(),
    x = ~Date,
    y = ~Measure,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines') %>%
    add_trace(y = ~Forecast,
              name = 'Forecast',
              line = list(dash = 'dot')) %>%
    # Update title and axes
    layout(title = paste("Forecasting phasings value in", input$cost_board),
           xaxis = list(title = '<b>Date</b>',
                        rangeslider = list(visible = TRUE,          # Enable the range slider
                                           bgcolor = phs_colours('phs-magenta-30'),   # Background color of the range slider
                                           bordercolor = phs_colours('phs-magenta'),    # Border color
                                           borderwidth = 2)
           ),
           yaxis = list(title = paste0("<b>Phasings</b>")),
           font = list(family = 'Arial'),
           legend = list(
             orientation = "h",
             x = 0.5,
             y = -0.7,
             xanchor = "center",
             yanchor = "top"
           ))
  
  plot
  
})

## Percentage change chart - number of paid items
gic_yearly_change <- reactive({
  
  forecast_gic %>%
    filter(Board == input$cost_board)
  
})


output$change_gic <- make_yoy_chart(gic_plot_data,
                                    "Gross Ingredient Cost (£)",
                                    reactive(input$cost_board))

## Percentage change chart - working day
cpi_yearly_change <- reactive({
  
  forecast_cpi %>%
    filter(Board == input$cost_board)
  
})


output$change_cpi <- make_yoy_chart(cpi_plot_data,
                                    "Cost per item",
                                    reactive(input$cost_board))

## Export data syntax and button ----
### Download handler
output$downloadData_gic <- downloadHandler(
  filename = function() {
    paste(input$cost_board, "-MonthlyGIC-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(gic_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_gic_quarterly <- downloadHandler(
  filename = function() {
    paste(input$cost_board, "-QuarterlyGIC-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(gic_quarterly_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_cpi <- downloadHandler(
  filename = function() {
    paste(input$cost_board, "-CostPerItem-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(cpi_plot_data(), file, row.names = FALSE)
  }
)

output$downloadData_phasings <- downloadHandler(
  filename = function() {
    paste(input$cost_board, "-Phasings-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(phasings_plot_data(), file, row.names = FALSE)
  }
)