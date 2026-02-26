### Data preparation file for showing forecast in dashboard
### Honours Project
### Date created: 01/08/2025
### Author: Liam Rooney

start_time <- Sys.time()

# 1. Load packages, key variables and data ----
options(scipen = 999)
## 1.1. Load packages ----
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(plotly)
library(openxlsx)
library(readxl)
library(officer)
library(glue)
library(devtools)
library(urca)
library(phsmethods)
library(tibble)

## 1.2. Initialise variables and file paths ----
'%!in%' <- function(x,y)!('%in%'(x,y))

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'

## 1.3. Load in data ----

### Healthboard code lookup
hb_code <- read_excel(glue('{path}/lookups/hb_code_lookup.xlsx'))

### Full dataset and joined with hb code lookup
hb_data <- rbind(
  read_excel(glue('{path}/data/healthboard/HB_Phasings_04_08.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_09_13.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_14_18.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_19_24.xlsx'))
  ) %>%
  mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
  left_join(hb_code, by = 'Disp Health Board Code') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board = `Disp Health Board Name`, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)

healthboards <- unique(hb_data$Board) 
healthboards <- healthboards[healthboards %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB')]

dates <- unique(hb_data$Date)

# Forecasting history file
history <- read_excel('accuracy/forecasting_history.xlsx') %>%
  pivot_longer(cols = 3:ncol(.),
               names_to = 'Date',
               values_to = 'value') %>%
  pivot_wider(names_from = 'Indicator',
              values_from = 'value') %>%
  mutate(Date = ceiling_date(as.Date(as.numeric(Date), origin = "1899-12-30"), unit = 'month') - days(1)) %>%
  left_join(hb_code %>% rename(Board = `Disp Health Board Code`), by = 'Board') %>%
  filter(!(is.na(`Disp Health Board Name`))) %>%
  select(-Board) %>%
  rename(Board = `Disp Health Board Name`) %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board, FY, everything())


# run_forecast <- function(end_date, fy) {
#   
#   dataframe <- data.frame(Date = NA,
#                           number = NA,
#                           FY = NA,
#                           HB = NA,
#                           check.names = FALSE)
#   
#   for (board in healthboards) {
#     
#     df <- data %>%
#       filter(`Disp Health Board Name` == board) %>%
#       select(Phasings)
#     
#     y <- ts(df,
#             start = c(2004, 4),
#             end = end_date,
#             frequency = 12)
#     
#     arima_fit <- Arima(y, order = c(0,1,1), seasonal = list(order = c(3,1,3), period = 12), method="ML")
#     
#     fc <- forecast(arima_fit, h = 12)
#     
#     fc <- data.frame(fc) %>%
#       select(number = Point.Forecast) %>%
#       mutate(FY = fy,
#              HB = board)
#     
#     fc <- tibble::rownames_to_column(fc, 'Date')
#     
#     dataframe <- dataframe %>%
#       rbind(fc)
#     
#   }
#   
#   dataframe <- dataframe[-1, ]
#   
#   dataframe <- dataframe %>%
#     mutate(Date = ceiling_date(dmy(paste0('01', Date)), unit = 'month') - days(1)) %>%
#     select(Board = HB,
#            FY,
#            Date,
#            second_forecast = number)
#   
#   final_df <- history %>%
#     left_join(dataframe, by = c('Board', 'FY', 'Date')) %>%
#     filter(FY == fy) %>%
#     mutate(`% error (second forecast)` = ((abs(Actual - second_forecast)) / second_forecast) * 100) %>%
#     mutate(error_comparison = `% error (second forecast)` - `% error`)
#   
# }
# 
# forecasted_data <- rbind(
#   run_forecast(c(2017, 3), '2017/18'),
#   run_forecast(c(2018, 3), '2018/19'),
#   run_forecast(c(2019, 3), '2019/20'),
#   run_forecast(c(2020, 3), '2020/21'),
#   run_forecast(c(2021, 3), '2021/22'),
#   run_forecast(c(2022, 3), '2022/23'),
#   run_forecast(c(2023, 3), '2023/24'),
#   run_forecast(c(2024, 3), '2024/25')
# )
# 
# saveRDS(forecasted_data, 'shiny/data/forecasted_data.rds')
# 
# # run auto.arima forecast
# run_auto_forecast <- function(end_date, fy) {
#   
#   dataframe <- data.frame(`Paid Date` = NA,
#                           number = NA,
#                           FY = NA,
#                           HB = NA,
#                           check.names = FALSE)
#   
#   for (board in healthboards) {
#     
#     df <- data %>%
#       filter(`Disp Health Board Name` == board) %>%
#       select(Phasings)
#     
#     y <- ts(df,
#             start = c(2004, 4),
#             end = end_date,
#             frequency = 12)
#     
#     arima_fit <- Arima(y, order = c(0,1,1), seasonal = list(order = c(3,1,3), period = 12), method="ML")
#     
#     fc <- forecast(arima_fit, h = 12)
#     
#     fc <- data.frame(fc) %>%
#       select(number = Point.Forecast) %>%
#       mutate(FY = fy,
#              HB = board)
#     
#     fc <- tibble::rownames_to_column(fc, 'Paid Date')
#     
#     dataframe <- dataframe %>%
#       rbind(fc)
#     
#   }
#   
#   dataframe <- dataframe[-1, ]
#   
#   dataframe <- dataframe %>%
#     mutate(Date = ceiling_date(dmy(paste0('01', `Paid Date`)), unit = 'month') - days(1)) %>%
#     select(Board = HB,
#            FY,
#            Date,
#            second_forecast = number)
#   
#   final_df <- history %>%
#     left_join(dataframe, by = c('Board', 'FY', 'Date')) %>%
#     filter(FY == fy) %>%
#     mutate(`% error (second forecast)` = ((abs(Actual - second_forecast)) / second_forecast) * 100) %>%
#     mutate(error_comparison = `% error (second forecast)` - `% error`)
#   
# }
# 
# auto_forecasted_data <- rbind(
#   run_forecast(c(2017, 3), '2017/18'),
#   run_forecast(c(2018, 3), '2018/19'),
#   run_forecast(c(2019, 3), '2019/20'),
#   run_forecast(c(2020, 3), '2020/21'),
#   run_forecast(c(2021, 3), '2021/22'),
#   run_forecast(c(2022, 3), '2022/23'),
#   run_forecast(c(2023, 3), '2023/24'),
#   run_forecast(c(2024, 3), '2024/25')
# )
# 
# saveRDS(forecasted_data, 'shiny/data/auto_forecasted_data.rds')
# 
# print(start_time - Sys.time())
# 
# 
# ########################
# 
# run_new_forecast <- function(end_date, fy) {
#   
#   dataframe <- data.frame(matrix(ncol = 4, nrow = 0))
#   colnames(dataframe) <- c('Date', 'number', 'FY', 'HB')
#   
#   for (board in healthboards) {
#     
#     df <- data %>%
#       filter(Board == board) %>%
#       select(Phasings)
#     
#     y <- ts(df,
#             start = c(2004, 4),
#             end = end_date,
#             frequency = 12)
#     
#     arima_fit <- Arima(y, order = c(6,0,4), seasonal = list(order = c(1,0,1), period = 12), method="ML") 
#     
#     fc <- forecast(arima_fit, h = 12)
#     
#     fc <- data.frame(fc) %>%
#       select(number = Point.Forecast) %>%
#       mutate(FY = fy,
#              HB = board)
#     
#     fc <- tibble::rownames_to_column(fc, 'Date')
#     
#     dataframe <- dataframe %>%
#       rbind(fc)
#     
#   }
#   
#   dataframe <- dataframe %>%
#     mutate(Date = ceiling_date(dmy(paste0('01', Date)), unit = 'month') - days(1)) %>%
#     select(Board = HB,
#            FY,
#            Date,
#            second_forecast = number)
#   
#   final_df <- data %>%
#     left_join(dataframe, by = c('Board', 'FY', 'Date')) %>%
#     filter(FY == fy) %>%
#     mutate(`% error` = ((abs(Phasings - second_forecast)) / second_forecast) * 100) #%>%
#     #mutate(error_comparison = `% error (second forecast)` - `% error`)
#   
# }
# 
# new_forecasted_data <- rbind(
#   # run_new_forecast(c(2017, 3), '2017/18'),
#   # run_new_forecast(c(2018, 3), '2018/19'),
#   # run_new_forecast(c(2019, 3), '2019/20'),
#   # run_new_forecast(c(2020, 3), '2020/21'),
#   # run_new_forecast(c(2021, 3), '2021/22'),
#   # run_new_forecast(c(2022, 3), '2022/23'),
#   # run_new_forecast(c(2023, 3), '2023/24'),
#   run_new_forecast(c(2024, 3), '2024/25')
# )
# 
# saveRDS(new_forecasted_data, 'shiny/data/new_forecasted_data.rds')
# 
# 
# df <- data %>%
#   filter(Board == 'NHS GREATER GLASGOW & CLYDE') %>%
#   select(Date, `Claim PD Number of Paid Items`) %>%
#   filter(Date > '2008-03-31' & Date < '2024-04-30')
# 
# y <- ts(df[,'Claim PD Number of Paid Items'],
#         start = c(2008, 4),
#         end = c(2024, 3),
#         frequency = 12)
# 
# 
# #arima_fit <- auto.arima(y, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowmean = FALSE)
# 
# arima_fit <- Arima(y, order = c(4,0,1), seasonal = list(order = c(3,1,2), period = 12), method="ML")
# checkresiduals(arima_fit)
# 
# fc <- forecast(arima_fit, h = 12)
# 
# fc$mean
# 
# 
# 
# fit1 <- Arima(y, order = c(0,1,1), seasonal = c(0,1,1)) 
# 
# fit1 %>%
#   residuals() %>%
#   ggtsdisplay()
# 
# fit2 <- Arima(y, order = c(0,1,2), seasonal = c(0,1,1)) 
# 
# fit2 %>%
#   residuals() %>%
#   ggtsdisplay()
# 
# fit3 <- Arima(y, order = c(0,1,3), seasonal = c(0,1,1)) 
# 
# fit3 %>%
#   residuals() %>%
#   ggtsdisplay()
# 
# fit4 <- Arima(y, order = c(6,0,4), seasonal = c(1,0,1)) 
# 
# 
# fit4 %>%
#   residuals() %>%
#   ggtsdisplay()
# 
# fc <- forecast(fit4, h = 12) %>% autoplot()
# fc
# 
# MuMIn::AICc(fit1, fit2, fit3, fit4)
# 
# fit1
# 
# checkresiduals(fit1)
# 
# fit2 <- Arima(y, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 12), method="ML")
# checkresiduals(fit2)
# 
# fit3 <- Arima(y, order = c(2,1,0), seasonal = list(order = c(1,1,0), period = 12), method="ML")
# checkresiduals(fit3)
# 
# 
# fit4 <- Arima(y, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 12), method="ML")
# checkresiduals(fit4)
# 
# coefficients <- fit4$coef
# standard_errors <- sqrt(diag(fit4$var.coef))
# 
# 
# 
# ggtsdisplay(y)
# 
# 
# ################
# df <- data %>%
#   filter(Board == 'NHS GREATER GLASGOW & CLYDE') %>%
#   select(Date, `Claim PD Number of Paid Items`) %>%
#   filter(Date > '2005-12-31' & Date < '2024-01-01')
# 
# y <- ts(df$`Claim PD Number of Paid Items`,
#         start = 2006,
#         frequency = 12)
# 
# comparison <- data %>%
#   filter(Board == 'NHS GREATER GLASGOW & CLYDE') %>%
#   select(Date, `Claim PD Number of Paid Items`) %>%
#   filter(!(Date < '2024-01-01'))
# 
# # Define grid for p, d, q
# p_values <- 0:9
# d_values <- 0:1
# q_values <- 0:9
# 
# # Storage for results
# results <- list()
# counter <- 1
# 
# # Loop through all combinations
# for (p in p_values) {
#   for (d in d_values) {
#     for (q in q_values) {
#       tryCatch({
#         # Fit ARIMA model
#         fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
#         
#         forecast <- forecast(fit, h = 12)
#         
#         forecast_list <- forecast$mean
#         
#         comparison <- comparison %>%
#           mutate(forecast = forecast_list) %>%
#           mutate(error = ((abs(forecast - `Claim PD Number of Paid Items`)) / forecast) * 100)
#         
#         # Save results: you can store log-likelihood, AIC, coefficients, etc.
#         results[[counter]] <- list(
#           p = p, d = d, q = q,
#           AIC = AIC(fit),
#           BIC = BIC(fit),
#           logLik = logLik(fit),
#           fit = fit,
#           forecast_error = mean(comparison$error)
#         )
#         counter <- counter + 1
#         
#       }, error = function(e) {
#         # Handle error and continue
#         message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
#       })    
#       }
#   }
# }
# 
# # Example: show best AIC
# best_model <- results[[which.min(sapply(results, function(x) x$forecast_error))]]
# 
# final_forecast <- Arima(y, order = c(best_model$p, best_model$d, best_model$q), seasonal = c(1,0,1))
# 
# final_forecast %>%
#   residuals() %>%
#   ggtsdisplay()
# 
# forecast <- forecast(final_forecast, h = 12) %>%
#   as.data.frame(.) %>%
#   tibble::rownames_to_column('Date')

start_time <- Sys.time()

dataframe <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(dataframe) <- c('Date', 'Point Forecast', 'Lo 80', 'Hi 80', 'Lo 95', 'Hi 95', 'Board')

for (board in healthboards) {
  
  df <- hb_data %>%
    filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(Date > '2005-12-31' & Date < '2024-01-01')
  
  y <- ts(df$`Claim PD Number of Paid Items`,
          start = 2006,
          frequency = 12)
  
  comparison <- hb_data %>%
    filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(!(Date < '2024-01-01'))
  
  # Define grid for p, d, q
  p_values <- 0:9
  d_values <- 0:1
  q_values <- 0:9
  
  # Storage for results
  results <- list()
  counter <- 1
  
  # Loop through all combinations
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        tryCatch({
          # Fit ARIMA model
          fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
          
          forecast <- forecast(fit, h = 24)
          
          forecast_list <- window(forecast$mean, end = c(2024, 12))
          
          comparison <- comparison %>%
            mutate(forecast = forecast_list) %>%
            mutate(error = ((abs(forecast - `Claim PD Number of Paid Items`)) / forecast) * 100)
          
          # Save results: you can store log-likelihood, AIC, coefficients, etc.
          results[[counter]] <- list(
            p = p, d = d, q = q,
            AIC = AIC(fit),
            BIC = BIC(fit),
            logLik = logLik(fit),
            fit = fit,
            forecast_error = mean(comparison$error)
          )
          counter <- counter + 1
          
        }, error = function(e) {
          # Handle error and continue
          message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
        })    
      }
    }
  }
  
  # Example: show best AIC
  best_model <- results[[which.min(sapply(results, function(x) x$forecast_error))]]
  
  final_forecast <- Arima(y, order = c(best_model$p, best_model$d, best_model$q), seasonal = c(1,0,1))
  
  final_forecast %>%
    residuals() %>%
    ggtsdisplay()
  
  fc <- forecast(final_forecast, h = 24) %>%
    as.data.frame(.) %>%
    tibble::rownames_to_column('Date') %>%
    mutate(Board = board)
  
  dataframe <- dataframe %>%
    rbind(fc)
  
}


dataframe <- dataframe %>%
  mutate(Date = ceiling_date(as.Date(paste(Date, "01"), format = "%b %Y %d"), unit = 'month') - days(1))

# number_of_items <- data %>%
#   select(Board, Date, FY, `Claim PD Number of Paid Items`) %>%
#   left_join(dataframe, by = c('Board', 'Date'))

number_of_items <- hb_data %>%
  select(Board, Date, FY, `Claim PD Number of Paid Items`) %>%
  full_join(dataframe, by = c('Board', 'Date'))

saveRDS(number_of_items, 'shiny/data/number_of_items.rds')

end_time <- Sys.time()

print(paste0('The amount of time taken to run this script was: ', (end_time - start_time), ' minutes.'))

# 
# plot <- plot_ly(
#   data = number_of_items %>% filter(Board == 'NHS FIFE'),
#   x = ~Date,
#   y = ~`Claim PD Number of Paid Items`,
#   name = 'Actual data', 
#   type = 'scatter', 
#   mode = 'lines') %>%
#   add_trace(y = ~`Point Forecast`,
#             name = 'Original forecast', 
#             line = list(dash = 'dot')) %>%
#   add_ribbons(ymin = ~`Lo 95`, ymax = ~`Hi 95`,
#               fillcolor = 'rgba(255, 0, 0, 0.2)', 
#               line = list(color = 'rgba(255, 0, 0, 0)'), 
#               name = '95% CI') %>%
#   add_ribbons(ymin = ~`Lo 80`, ymax = ~`Hi 80`,
#               line = list(color = 'rgba(0,0,0,0)'), 
#               fillcolor = 'rgba(100,100,200,0.2)', 
#               name = '80% CI')


start_time <- Sys.time()

results_list <- tibble(board = character(),
                       p = numeric(), 
                       d = numeric(), 
                       q = numeric(), 
                       AIC = double(), 
                       BIC = double(), 
                       AICc = double(),
                       ME = double(), 
                       MPE = double(), 
                       MAPE = double(),
                       ACF1 = double(),
                       p_value = double(),
                       forecast_error = double())

for (board in healthboards) {
  
  df <- hb_data %>%
    filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(Date > '2005-12-31' & Date < '2024-01-01')
  
  y <- ts(df$`Claim PD Number of Paid Items`,
          start = 2006,
          frequency = 12)
  
  comparison <- hb_data %>%
    filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(!(Date < '2024-01-01'))
  
  # Define grid for p, d, q
  p_values <- 0:9
  d_values <- 0:1
  q_values <- 0:9
  
  # Storage for results
  results <- tibble(board = character(),
                    p = numeric(), 
                    d = numeric(), 
                    q = numeric(), 
                    AIC = double(), 
                    BIC = double(), 
                    AICc = double(), 
                    ME = double(), 
                    MPE = double(), 
                    MAPE = double(),
                    ACF1 = double(),
                    p_value = double(),
                    forecast_error = double())
  counter <- 1
  
  # Loop through all combinations
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        tryCatch({
          # Fit ARIMA model
          fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
          
          p_value <- (fit %>% checkresiduals(plot = FALSE))$p.value
          
          forecast <- forecast(fit, h = 24)
          
          accuracylist <- as_tibble(accuracy(forecast))
          
          forecast_list <- window(forecast$mean, end = c(2024, 12))
          
          comparison <- comparison %>%
            mutate(forecast = forecast_list) %>%
            mutate(error = ((abs(forecast - `Claim PD Number of Paid Items`)) / forecast) * 100)
          
          # Save results: you can store log-likelihood, AIC, coefficients, etc.

          results <- results %>%
            add_row(
              board = board,
              p = p, d = d, q = q,
              AIC = AIC(fit),
              BIC = BIC(fit),
              AICc = fit$aicc,
              ME = accuracylist$ME,
              MPE = accuracylist$MPE,
              MAPE = accuracylist$MAPE,
              ACF1 = accuracylist$ACF1,
              p_value = p_value,
              forecast_error = mean(comparison$error)
          )
          counter <- counter + 1
          
        }, error = function(e) {
          # Handle error and continue
          message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
        })    
      }
    }
  }
  
  
  results_list <- results_list %>%
    rbind(results)
  
}

end_time <- Sys.time()

print(paste0('The amount of time taken to run this script was: ', (end_time - start_time), ' minutes.'))

saveRDS(results_list, 'shiny/data/results_list.rds')


## Scotland forecast
scotland_data <- rbind(
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_04_08.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_09_13.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_14_18.xlsx')),
  read_excel(glue('{path}/data/scotland/Scotland_Phasings_19_24.xlsx'))
) %>%
  #mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
  mutate(Date = as.Date(`Paid Date`),
         Board = 'SCOTLAND') %>%
  #left_join(hb_code, by = 'Disp Health Board Code') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)

healthboards <- unique(scotland_data$Board) 
healthboards <- healthboards[healthboards %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB')]

dates <- unique(scotland_data$Date)

start_time <- Sys.time()

results_list <- tibble(board = character(),
                       p = numeric(), 
                       d = numeric(), 
                       q = numeric(), 
                       AIC = double(), 
                       BIC = double(), 
                       AICc = double(),
                       ME = double(), 
                       MPE = double(), 
                       MAPE = double(),
                       ACF1 = double(),
                       p_value = double(),
                       forecast_error = double())

  df <- scotland_data %>%
    #filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(Date > '2005-12-31' & Date < '2024-01-01')
  
  y <- ts(df$`Claim PD Number of Paid Items`,
          start = 2006,
          frequency = 12)
  
  comparison <- scotland_data %>%
    #filter(Board == board) %>%
    select(Date, `Claim PD Number of Paid Items`) %>%
    filter(!(Date < '2024-01-01') & Date < '2025-01-01')
  
  # Define grid for p, d, q
  p_values <- 0:9
  d_values <- 0:1
  q_values <- 0:9
  
  # Storage for results
  results <- tibble(board = character(),
                    p = numeric(), 
                    d = numeric(), 
                    q = numeric(), 
                    AIC = double(), 
                    BIC = double(), 
                    AICc = double(), 
                    ME = double(), 
                    MPE = double(), 
                    MAPE = double(),
                    ACF1 = double(),
                    p_value = double(),
                    forecast_error = double())
  counter <- 1
  
  # Loop through all combinations
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        tryCatch({
          # Fit ARIMA model
          fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
          
          p_value <- (fit %>% checkresiduals(plot = FALSE))$p.value
          
          forecast <- forecast(fit, h = 24)
          
          accuracylist <- as_tibble(accuracy(forecast))
          
          forecast_list <- window(forecast$mean, end = c(2024, 12))
          
          comparison <- comparison %>%
            mutate(forecast = forecast_list) %>%
            mutate(error = ((abs(forecast - `Claim PD Number of Paid Items`)) / forecast) * 100)
          
          # Save results: you can store log-likelihood, AIC, coefficients, etc.
          
          results <- results %>%
            add_row(
              board = 'SCOTLAND',
              p = p, d = d, q = q,
              AIC = AIC(fit),
              BIC = BIC(fit),
              AICc = fit$aicc,
              ME = accuracylist$ME,
              MPE = accuracylist$MPE,
              MAPE = accuracylist$MAPE,
              ACF1 = accuracylist$ACF1,
              p_value = p_value,
              forecast_error = mean(comparison$error)
            )
          counter <- counter + 1
          
        }, error = function(e) {
          # Handle error and continue
          message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
        })    
      }
    }
  }
  
  
  results_list <- results_list %>%
    rbind(results)
  

end_time <- Sys.time()

print(paste0('The amount of time taken to run this script was: ', (end_time - start_time), ' minutes.'))

saveRDS(results_list, 'shiny/data/scotland_results_list.rds')





