
# ############## Accuracy Server ######################

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

items <- reactive({
  
  data <- number_of_items %>%
    filter(Board == input$board,
           Date > '2017-01-01')
  
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

output$forecast_plot <- renderPlotly({
  
  plot <- plot_ly(
    data = items(),
    x = ~Date,
    y = ~`Claim PD Number of Paid Items`,
    name = 'Actual data',
    type = 'scatter',
    mode = 'lines') %>%
    add_trace(y = ~`Point Forecast`,
              name = 'Original forecast',
              line = list(dash = 'dot')) %>%
    add_ribbons(ymin = ~`Lo 95`, ymax = ~`Hi 95`,
                fillcolor = 'rgba(255, 0, 0, 0.2)',
                line = list(color = 'rgba(255, 0, 0, 0)'),
                name = '95% CI') %>%
    add_ribbons(ymin = ~`Lo 80`, ymax = ~`Hi 80`,
                line = list(color = 'rgba(0,0,0,0)'),
                fillcolor = 'rgba(100,100,200,0.2)',
                name = '80% CI')
  
})

