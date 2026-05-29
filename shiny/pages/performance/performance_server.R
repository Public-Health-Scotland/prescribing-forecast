# ############## Forecast Performance Server ######################

## 1.1. Wrangle table for output
forecast_performance <- sarima_v2.1_performance %>%
  select(board, p, d, q, p_value, forecast_error, type) %>%
  mutate(Accuracy = 100 - forecast_error) %>%
  filter(type == "Cost per item") %>%
  arrange(forecast_error) %>%
  mutate(board = factor(board, levels = unique(board)),
         group = if_else(board == "SCOTLAND", "SCOTLAND", "Other"))

accuracy_chart <- plot_ly(
  forecast_performance,
  x = ~board,
  y = ~forecast_error,
  type = "bar",
  color = ~group,
  colors = c("SCOTLAND" = phs_colours("phs-purple"),
             "Other"    = phs_colours("phs-blue"))) %>%
  layout(
    xaxis = list(title = "Prescribing Health Board"),
    yaxis = list(title = "Forecast Error"),
    showlegend = FALSE
    )







