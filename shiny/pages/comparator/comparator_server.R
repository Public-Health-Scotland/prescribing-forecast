
# ############## Comparator Tab Server ######################

palette <- phs_colour_values

# 1. Disabling and locking aggregation dropdown to "Monthly" if measure choice is cost per item ----
observeEvent(input$comparator_measure, {
  
  if (input$comparator_measure == "Cost per item") {
    
    choices <- "Monthly"
    
    updateSelectizeInput(session, "comparator_agg",
                         choices = choices,
                         selected = choices)
    
    shinyjs::disable("comparator_agg")
    
  } else {
    
    choices <- c("Monthly", "Quarterly")
    
    updateSelectizeInput(session, "comparator_agg",
                         choices = choices,
                         selected = choices[1])
    
    shinyjs::enable("comparator_agg")
  }
  
})


## 1.1. Filter data based on board input

### 1.1.1. Items ----
items_first_board_data <- filter_data(forecast_items,
                                      reactive(input$first_board))

items_second_board_data <- filter_data(forecast_items,
                                       reactive(input$second_board))

items_quarterly_first_board_data <- filter_data(forecast_items_quarterly,
                                                reactive(input$first_board))

items_quarterly_second_board_data <- filter_data(forecast_items_quarterly,
                                                 reactive(input$second_board))

### 1.1.2. GIC ----
gic_first_board_data <- filter_data(forecast_gic,
                                    reactive(input$first_board))

gic_second_board_data <- filter_data(forecast_gic,
                                     reactive(input$second_board))

gic_quarterly_first_board_data <- filter_data(forecast_gic_quarterly,
                                              reactive(input$first_board))

gic_quarterly_second_board_data <- filter_data(forecast_gic_quarterly,
                                               reactive(input$second_board))

### 1.1.2. Cost per item ----
cpi_first_board_data <- filter_data(forecast_cpi,
                                    reactive(input$first_board))

cpi_second_board_data <- filter_data(forecast_cpi,
                                     reactive(input$second_board))

cpi_quarterly_first_board_data <- filter_data(forecast_cpi_quarterly,
                                              reactive(input$first_board))

cpi_quarterly_second_board_data <- filter_data(forecast_cpi_quarterly,
                                               reactive(input$second_board))

## 1.3. Render plots
### 1.3.1. Items charts
output$items_plot_first_board <- make_chart(items_first_board_data,
                                            "Number of Paid Items",
                                            "monthly",
                                            reactive(input$first_board))

output$items_plot_second_board <- make_chart(items_second_board_data,
                                             "Number of Paid Items",
                                             "monthly",
                                             reactive(input$second_board))

output$items_quarterly_plot_first_board <- make_chart(items_quarterly_first_board_data,
                                                      "Number of Paid Items",
                                                      "quarterly",
                                                      reactive(input$first_board))

output$items_quarterly_plot_second_board <- make_chart(items_quarterly_second_board_data,
                                                       "Number of Paid Items",
                                                       "quarterly",
                                                       reactive(input$second_board))



### 1.3.1. GIC charts
output$gic_plot_first_board <- make_chart(gic_first_board_data,
                                          "Gross Ingredient Cost (£)",
                                          "monthly",
                                          reactive(input$first_board))

output$gic_plot_second_board <- make_chart(gic_second_board_data,
                                           "Gross Ingredient Cost (£)",
                                           "monthly",
                                           reactive(input$second_board))

output$gic_quarterly_plot_first_board <- make_chart(gic_quarterly_first_board_data,
                                                    "Gross Ingredient Cost (£)",
                                                    "quarterly",
                                                    reactive(input$first_board))

output$gic_quarterly_plot_second_board <- make_chart(gic_quarterly_second_board_data,
                                                     "Gross Ingredient Cost (£)",
                                                     "quarterly",
                                                     reactive(input$second_board))

### 1.3.1. Cost per item charts
output$cpi_plot_first_board <- make_chart(cpi_first_board_data,
                                          "Cost per item",
                                          "monthly",
                                          reactive(input$first_board))

output$cpi_plot_second_board <- make_chart(cpi_second_board_data,
                                           "Cost per item",
                                           "monthly",
                                           reactive(input$second_board))

output$cpi_quarterly_plot_first_board <- make_chart(cpi_quarterly_first_board_data,
                                                    "Cost per item",
                                                    "quarterly",
                                                    reactive(input$first_board))

output$cpi_quarterly_plot_second_board <- make_chart(cpi_quarterly_second_board_data,
                                                     "Cost per item",
                                                     "quarterly",
                                                     reactive(input$second_board))

