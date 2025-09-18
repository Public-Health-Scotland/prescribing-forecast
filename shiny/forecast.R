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
library(fpp)
library(urca)
library(phsmethods)
library(tibble)
library(bizdays)

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
  read_excel(glue('{path}/data/healthboard/HB_Phasings_19_24.xlsx')),
  read_excel(glue('{path}/data/healthboard/HB_Phasings_25.xlsx'))
) %>%
  mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') + months(3) - days(1)) %>%
  left_join(hb_code, by = 'Disp Health Board Code') %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board = `Disp Health Board Name`, Date, FY, `Claim PD Number of Paid Items`)

scotland_data <- hb_data %>%
  group_by(Date, FY) %>%
  summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`)) %>%
  mutate(Board = 'SCOTLAND') %>%
  select(Board, everything()) %>%
  ungroup()

hb_data <- hb_data %>%
  rbind(scotland_data)
  

# scotland_data <- rbind(
#   read_excel(glue('{path}/data/scotland/Scotland_Phasings_04_08.xlsx')),
#   read_excel(glue('{path}/data/scotland/Scotland_Phasings_09_13.xlsx')),
#   read_excel(glue('{path}/data/scotland/Scotland_Phasings_14_18.xlsx')),
#   read_excel(glue('{path}/data/scotland/Scotland_Phasings_19_24.xlsx'))
# ) %>%
#   #mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
#   mutate(Date = as.Date(`Paid Date`),
#          Board = 'SCOTLAND') %>%
#   #left_join(hb_code, by = 'Disp Health Board Code') %>%
#   mutate(FY = extract_fin_year(Date)) %>%
#   select(Board, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)

healthboards <- unique(hb_data$Board) 
healthboards <- healthboards[healthboards %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB', 'SCOTLAND')]

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

# 2. Run forecast ----

## 2.1. Initialise tables and variables ----

latest_date <- max(hb_data$Date)
max_date <- max(hb_data$Date) - months(12)

## ---- only uncomment if running the forecast again ----

## Calculate business days for each month, keep commented out if don't want per working day figure
dates <- unique(hb_data$Date)

start_date <- min(dates)
end_date <- max(dates)

holidays <- c(
  # 2004 holidays
  '2004-01-01', '2004-01-02', '2004-04-09', '2004-05-03', '2004-05-31', '2004-08-02', '2004-11-30', '2004-12-27', '2004-12-28',
  # 2005 holidays
  '2005-01-03', '2005-01-04', '2005-03-25', '2005-05-02', '2005-05-30', '2005-08-01', '2005-11-30', '2005-12-26', '2005-12-27',
  # 2006 holidays
  '2006-01-02', '2006-01-03', '2006-04-14', '2006-05-01', '2006-05-29', '2006-08-07', '2006-11-30', '2006-12-25', '2006-12-26',
  # 2007 holidays
  '2007-01-01', '2007-01-02', '2007-04-06', '2007-05-07', '2007-05-28', '2007-08-06', '2007-11-30', '2007-12-25', '2007-12-26',
  # 2008 holidays
  '2008-01-01', '2008-01-02', '2008-03-21', '2008-05-05', '2008-05-26', '2008-08-04', '2008-12-01', '2008-12-25', '2008-12-26',
  # 2009 holidays
  '2009-01-01', '2009-01-02', '2009-04-10', '2009-05-04', '2009-05-25', '2009-08-03', '2009-11-30', '2009-12-25', '2009-12-28',
  # 2010 holidays
  '2010-01-01', '2010-01-04', '2010-04-02', '2010-05-03', '2010-05-31', '2010-08-02', '2010-11-30', '2010-12-27', '2010-12-28',
  # 2011 holidays
  '2011-01-03', '2011-01-04', '2011-04-22', '2011-04-29', '2011-05-02', '2011-05-30', '2011-08-01', '2011-11-30', '2011-12-26', '2011-12-27',
  # 2012 holidays
  '2012-01-02', '2012-01-03', '2012-04-06', '2012-05-07', '2012-06-04', '2012-06-05', '2012-08-06', '2012-11-30', '2012-12-25', '2012-12-26',
  # 2013 holidays
  '2013-01-01', '2013-01-02', '2013-03-29', '2013-05-06', '2013-05-27', '2013-08-05', '2013-12-02', '2013-12-25', '2013-12-26',
  # 2014 holidays
  '2014-01-01', '2014-01-02', '2014-04-18', '2014-05-05', '2014-05-26', '2014-08-04', '2014-12-01', '2014-12-25', '2014-12-26',
  # 2015 holidays
  '2015-01-01', '2015-01-02', '2015-04-03', '2015-05-04', '2015-05-25', '2015-08-03', '2015-11-30', '2015-12-25', '2015-12-28',
  # 2016 holidays
  '2016-01-01', '2016-01-04', '2016-03-25', '2016-05-02', '2016-05-30', '2016-08-01', '2016-11-30', '2016-12-26', '2016-12-27',
  # 2017 holidays
  '2017-01-02', '2017-01-03', '2017-04-14', '2017-05-01', '2017-05-29', '2017-08-07', '2017-11-30', '2017-12-25', '2017-12-26',
  # 2018 holidays
  '2018-01-01', '2018-01-02', '2018-03-30', '2018-05-07', '2018-05-28', '2018-08-06', '2018-11-30', '2018-12-25', '2018-12-26',
  # 2019 holidays
  '2019-01-01', '2019-01-02', '2019-04-19', '2019-05-06', '2019-05-27', '2019-08-05', '2019-12-02', '2019-12-25', '2019-12-26',
  # 2020 holidays
  '2020-01-01', '2020-01-02', '2020-04-10', '2020-05-08', '2020-05-25', '2020-08-03', '2020-11-30', '2020-12-25', '2020-12-28',
  # 2021 holidays
  '2021-01-01', '2021-01-04', '2021-04-02', '2021-05-03', '2021-05-31', '2021-08-02', '2021-11-30', '2021-12-27', '2021-12-28',
  # 2022 holidays
  '2022-01-03', '2022-01-04', '2022-04-15', '2022-05-02', '2022-06-02', '2022-06-03', '2022-08-01', '2022-09-19', '2022-11-30', '2022-12-26', '2022-12-27',
  # 2023 holidays
  '2023-01-02', '2023-01-03', '2023-04-07', '2023-05-01', '2023-05-08', '2023-05-29', '2023-08-07', '2023-11-30', '2023-12-25', '2023-12-26',
  # 2024 holidays
  '2024-01-01', '2024-01-02', '2024-03-29', '2024-05-06', '2024-05-27', '2024-08-05', '2024-12-02', '2024-12-25', '2024-12-26',
  # 2025 holidays
  '2025-01-01', '2025-01-02', '2025-04-18', '2025-05-05', '2025-05-26', '2025-08-04', '2025-12-01', '2025-12-25', '2025-12-26'
  # add in holidays for further years below
  
  
)

saveRDS(holidays, 'shiny/data/holidays.rds')

# Create a calendar excluding weekends and holidays
Scotland <- create.calendar(name = "Scotland", weekdays = c("saturday", "sunday"), holidays = holidays)

# Generate a sequence of months
months <- seq(
  from = as.Date(format(start_date, "%Y-%m-01")),
  to   = as.Date(format(end_date, "%Y-%m-01")),
  by   = "month"
)

# Calculate business days for each month
business_days <- sapply(months, function(month) {
  first_day <- as.Date(format(month, "%Y-%m-01"))
  last_day  <- ceiling_date(first_day, "month") - days(1)
  bizdays::bizdays(first_day, last_day, Scotland)
})

# Combine results into a data frame
result <- data.frame(
  Date = as.Date(format(ceiling_date(months, "month") - days(1), "%Y-%m-%d")),
  Business_Days = business_days
)

saveRDS(result, 'shiny/data/business_days_lookup.rds')


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

new_hb_data <- hb_data %>% # comment code below out depending on forecast
  left_join(result, by = 'Date') %>%
  mutate(`Paid Items per Working Day` = `Claim PD Number of Paid Items` / Business_Days)

## 2.2. Forecast function for number of working days ----
run_forecast <- function(hbs, years) {
  
  yearly_results <- results_list
  full_results <- results_list %>% add_column(year = character())
  
  for (year in years) {
    
    y <- year
    
    diff <- year(Sys.time()) - y
    
    MaxDate <- new_hb_data %>%
      filter(Date == max(Date) - months(12 * diff))
    
    window_max <- unique(MaxDate$Date)
    MaxDate <- window_max - months(12)
    
    for (board in hbs) {
      
      df <- new_hb_data %>%
        filter(Board == board) %>%
        #select(Date, `Claim PD Number of Paid Items`) %>%
        select(Date, `Paid Items per Working Day`) %>% # comment in or out depending on what number you want
        filter(Date > '2005-12-31' & Date < MaxDate + days(1))
      
      y <- ts(
        #df$`Claim PD Number of Paid Items`,      
        df$`Paid Items per Working Day`,
        start = 2006,
        frequency = 12
      )
      
      comparison <- new_hb_data %>%
        filter(Board == board) %>%
        #select(Date, `Claim PD Number of Paid Items`) %>%
        select(Date, `Paid Items per Working Day`) %>%
        filter(Date > MaxDate & Date < window_max + days(1))
      
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
              
              forecast_list <- window(forecast$mean, end = c(year(window_max), month(window_max)))
              
              comparison <- comparison %>%
                mutate(forecast = forecast_list) %>%
                mutate(error = ((abs(forecast - `Paid Items per Working Day`)) / forecast) * 100)
              
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
      
      
      yearly_results <- yearly_results %>%
        rbind(results %>%
                filter(forecast_error == min(forecast_error)))
      
    }
    
    full_results <- full_results %>%
      rbind(yearly_results %>% mutate(year = year))
    
  }
  
  
  return(full_results)
  
  
}

## 2.3. Run forecasts here ----
forecast_2022 <- run_forecast(healthboards, c(2023))
forecast_2023 <- run_forecast(healthboards, c(2024))
forecast_2024 <- run_forecast(healthboards, c(2025))

### 2.3.1. Combine and save out forecasts
forecast_22_23_24 <- rbind(
  forecast_2022,
  forecast_2023,
  forecast_2024
)

saveRDS(forecast_22_23_24, 'shiny/data/forecast_22_23_24.rds')

end_time <- Sys.time()

print(paste0('The amount of time taken to run this script was: ', (end_time - start_time), ' minutes.'))

# Uncomment here if wanting to save file for per working day data
saveRDS(results_list, 'shiny/data/performance_working_day.rds')

## 2.4. Create full table of forecasted data with confidence intervals
results_list <- readRDS('shiny/data/performance_working_day.rds')

combined_forecast <- data.frame()
boards <- unique(results_list$board)

for (x in boards) {
  
  df <- new_hb_data %>%
    filter(Board == x) %>%
    select(Date, `Paid Items per Working Day`) %>%
    #select(Date, `Claim PD Number of Paid Items`) %>%
    filter(Date > '2005-12-31' & Date < max_date + days(1))
  
  y <- ts(df$`Paid Items per Working Day`,
          start = 2006,
          frequency = 12)
  
  filtered_list <- results_list %>%
    filter(board == x)
  
  p <- filtered_list$p
  d <- filtered_list$d
  q <- filtered_list$q
  
  fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
  
  forecast <- forecast(fit, h = 24)
  
  combined_forecast <- combined_forecast %>%
    rbind(data.frame(
      Board = x,
      Date = ceiling_date(as.Date(time(forecast$mean)), unit = 'month') - days(1),
      Forecast = as.numeric(forecast$mean),
      Lower_80 = as.numeric(forecast$lower[, 1]),
      Upper_80 = as.numeric(forecast$upper[, 1]),
      Lower_95 = as.numeric(forecast$lower[, 2]),
      Upper_95 = as.numeric(forecast$upper[, 2])
    ))
  
}

## 2.5. Create a Scotland total based on the forecasted values from each board, join to original table
scotland_result <- combined_forecast %>%
  filter(!(Board == 'SCOTLAND')) %>%
  group_by(Date) %>%
  summarise(Forecast = sum(Forecast),
            Lower_80 = sum(Lower_80),
            Upper_80 = sum(Upper_80),
            Lower_95 = sum(Lower_95),
            Upper_95 = sum(Upper_95)) %>%
  mutate(Board = 'SCOTLAND') %>%
  select(Board, everything())
  
combined_forecast <- combined_forecast %>%
  filter(!(Board == 'SCOTLAND')) %>%
  rbind(scotland_result)

result <- new_hb_data %>%
  full_join(combined_forecast)

saveRDS(result, 'shiny/data/aggregated_forecast_wd.rds')

## 2.6. Join historical data with forecasted data
# final_result <- hb_data %>%
#   select(Board, Date, FY, `Claim PD Number of Paid Items`) %>%
#   right_join(result, by = c('Board', 'Date'))

# 3. Generate plots ----





end_time <- Sys.time()


