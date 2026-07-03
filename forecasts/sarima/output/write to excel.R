# forecast <- readRDS('shiny/forecasts/sarima/output/NHSTAYSIDE-forecast-run-3.rds') %>%
#   filter(Historical_Data == "12 months of historical data",
#          Year == 2025,
#          F_Horizon == 48,
#          Arima_Error == FALSE) %>%
#   select(-c(Historical_Data, Year, F_Horizon, Arima_Error))

forecast_performance <- readRDS('shiny/forecasts/sarima/output/12 months/run 3/forecast-performance-2026-02-26.rds') %>%
  # filter(Historical_Data == "12 months of historical data",
  #        Year == 2025,
  #        F_Horizon == 48,
  #        Arima_Error == FALSE) %>%
  select(-c(historical_data, suffix, forecast_date))

library(readxl)
library(openxlsx)

write.xlsx(forecast_performance, 'shiny/forecasts/sarima/output/forecast-performance-2026-02-26.xlsx')
