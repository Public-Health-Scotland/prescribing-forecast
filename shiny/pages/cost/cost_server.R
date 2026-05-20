
# ############## Results Server ######################

palette <- phs_colour_values

## 1.1. Filter data based on board input

gic_plot_data <- filter_data(forecast_gic,
                             reactive(input$cost_board))

gic_quarterly_plot_data <- filter_data(forecast_gic_quarterly,
                                       reactive(input$cost_board))

cpi_plot_data <- filter_data(forecast_cpi,
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
