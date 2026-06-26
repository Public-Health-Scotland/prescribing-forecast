### SARIMA Forecast
### Honours Project
### Date last updated: 08/06/2026
### Author: Liam Rooney
### Model: v2.1

### Changes made to previous model version (v2.0):
### 1. Time series starting boundary moved from 2004 to 2011
###    - takes into account introduction of free prescriptions in 2011
###    - also eliminates any effect of the Argyll & Clyde HB split (circa 2006) from showing in the data

### Key notes for running this script:
### Once the variables at lines 48, 49, 50, 56, 70, 72 and 74 have been defined by the user: 
### 1. Click Source --> Source as Workbench Job --> define the memory parameters
### 2. Sit back and let the forecast work its magic! 

options(scipen = 999)

start <- Sys.time()

# 1. Load packages, key variables and data ----

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
library(bizdays)
library(doParallel)
library(foreach)
library(ggtime)
library(tsibble)
library(feasts)

## 1.2. Initialise variables and file paths ----
'%!in%' <- function(x,y)!('%in%'(x,y))

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'
setwd('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast')

# These are the columns that will be forecasted
# WARNING: The more columns included, the longer the runtime will be. It is recommended that this script is set to run as a Workbench job.
working_day = FALSE

# Comment columns out as required
columns_to_forecast = c(
  'Claim PD Number of Paid Items',
  'Claim PD Paid GIC excl. BB',
  'Cost per item'
)

# This variable decides what years to cap the time series at upon each run
# Time series begins in 2011

# Month of the year that time series is capped on will be guided by the last month of data loaded into PIS

# Set this year based on the last month loaded into nDCVP
years_to_run <- c(2025)
historical_data = 12 # number of months for forecast to be compared with against historical data (if set at 12, forecast vs. observed for previous year will give an average absolute error)
run_number = 999 # update each time

healthboards <- c("NHS AYRSHIRE & ARRAN", "NHS BORDERS", "NHS DUMFRIES & GALLOWAY", "NHS FIFE",
                  "NHS FORTH VALLEY", "NHS GRAMPIAN", "NHS GREATER GLASGOW & CLYDE", "NHS HIGHLAND", 
                  "NHS LANARKSHIRE", "NHS LOTHIAN", "NHS TAYSIDE", "NHS WESTERN ISLES", "NHS ORKNEY", 
                  "NHS SHETLAND", "SCOTLAND")

# 2. Read in data and some processing ----
data <- read.csv(paste0(glue('shiny/forecasts/data/Historical Data.csv')), check.names = FALSE) %>% # REMINDER: check that this is the correct file from the BOXI run you have just scheduled
  filter(`Presc Health Board Name` %in% healthboards)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
#data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
#data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values

board_data <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31' & `Paid Date` < "2025-04-01") # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions

rm(data)

## 2.1. Aggregate and join Scotland data ----
scotland_data <- board_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(`Presc Health Board Name` = 'SCOTLAND') %>%
  select(`Presc Health Board Name`, everything())

board_data <- board_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # double check data is arranged by board and date 
  arrange(`Presc Health Board Name`, `Paid Date`) #%>%
#filter(`Paid Date` < "2026-01-01")

board_data$`Cost per item`[board_data$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values

## 2.2 Create healthboard and date variables ----
healthboards <- unique(board_data$`Presc Health Board Name`) 
dates <- unique(board_data$`Paid Date`)

# # 2.3 Investigating time-series
# acf_pacf_items <- scotland_data %>%
#   #filter(Board == "NHS AYRSHIRE & ARRAN") %>%
#   arrange(`Paid Date`) %>%
#   filter(`Paid Date` > "2011-01-01" & `Paid Date` < "2026-01-01") %>%
#   select(`Paid Date`, `Claim PD Number of Paid Items`) %>%
#   mutate(Month = yearmonth(`Paid Date`)) %>%
#   as_tsibble(index = Month) %>%
#   #gg_tsdisplay(`Claim PD Number of Paid Items`)
#   mutate(
#     diff_1 = difference(`Claim PD Number of Paid Items`, 1),
#     diff_12 = difference(diff_1, 12)
#   ) #%>%
#   #gg_tsdisplay(diff_12)
# 
# acf_pacf_items %>%
#   #ACF(`Claim PD Number of Paid Items`) %>%
#   #ACF(diff_1) %>%
#   ACF(diff_12) %>%
#   autoplot() +
#   theme(plot.title = element_text(size = 18),
#         axis.title.x = element_text(size = 14), # X-axis title
#         axis.title.y = element_text(size = 14), # Y-axis title
#         axis.text.x = element_text(size = 12), # X-axis labels
#         axis.text.y = element_text(size = 12)) # Y-axis labels

# data <- board_data %>%
#   filter(`Presc Health Board Name` == 'SCOTLAND') %>%
#   select(`Paid Date`, `Claim PD Number of Paid Items`)
# 
# ts_data <- ts(data$`Claim PD Number of Paid Items`, start = c(2010, 4), frequency = 12)
# 
# autoplot(ts_data) +
#   labs(title = "Scotland: number of paid prescription items",
#        y="Number of Paid Items")
# 
# as_tsibble(ts_data) %>%
#   gg_tsPresclay(difference(value, 12),
#                plot_type='partial', lag=36) +
#   labs(title="Seasonally differenced", y="")

# 3. Run forecast ----

## 3.1. Initialise key variables and tables to be used ----

latest_date <- max(board_data$`Paid Date`)
max_date <- latest_date - months(historical_data)

## 3.2. Calculating business days - controlled by working_day logical variable ----

if (working_day == TRUE) {
  
  ## Calculate business days for each month, keep commented out if don't want per working day figure. Holidays required to be updated at end of 2026.
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
    '2025-01-01', '2025-01-02', '2025-04-18', '2025-05-05', '2025-05-26', '2025-08-04', '2025-12-01', '2025-12-25', '2025-12-26',
    # 2026 holidays
    '2026-01-01', '2026-01-02', '2026-04-03', '2026-05-04', '2026-05-25', '2026-08-03', '2026-11-30', '2026-12-25', '2026-12-28',
    "2027-01-01"
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
  
  saveRDS(result, "shiny/forecasts/data/business_days_lookup.rds")
  
  result <- read_rds(glue("{path}/shiny/data/business_days_lookup.rds"))
  
}

start_time <- Sys.time()

# Create empty results_list with required columns and correct data types
results_list <- tibble(board = character(),
                       p = numeric(),
                       q = numeric(),
                       P = numeric(),
                       Q = numeric(),
                       AIC = double(),
                       BIC = double(),
                       AICc = double(),
                       ME = double(),
                       MPE = double(),
                       MAPE = double(),
                       ACF1 = double(),
                       p_value = double(),
                       forecast_error = double())

max_year <- max(year(board_data$`Paid Date`))

## 3.3. SARIMA Forecast (function) ---- 

run_forecast <- function(columns, hbs, years) {
  
  start_time <- Sys.time()
  
  full_results <- tibble()  # Initialise cleanly
  
  for (column in columns) {
    
    column_results <- tibble()
    
    for (year in years) {
      
      yearly_results <- tibble()
      
      # Year adjustment
      diff <- year(latest_date) - year
      
      date_adj <- board_data %>%
        filter(`Paid Date` == max(`Paid Date`) - months(12 * diff))
      
      window_max <- unique(date_adj$`Paid Date`)
      MaxDate <- window_max - months(historical_data)
      
      for (board in hbs) {
        
        df <- board_data %>%
          filter(`Presc Health Board Name` == board) %>%
          select(`Paid Date`, column) %>% # can only select date column and predictor variable for use in time series
          filter(`Paid Date` > '2011-03-31' & `Paid Date` < MaxDate + days(1))
        
        y_ts <- ts(df[[column]], start = c(2011,4), frequency = 12) # start of time series limited to post-2011 due to introduction of free prescriptions, frequency is 12 to indicate monthly time-series
        
        comparison <- board_data %>%
          filter(`Presc Health Board Name` == board) %>%
          select(`Paid Date`, column) %>%
          filter(`Paid Date` > MaxDate & `Paid Date` < window_max + days(1))
        
        # # SARIMA grid
        # p_values <- 0:9
        # d_values <- 0:1
        # q_values <- 0:9
        # 
        # results <- tibble()
        # 
        # for (p in p_values) {
        #   for (d in d_values) {
        #     for (q in q_values) {
        #       tryCatch({
        #         fit <- Arima(y_ts, order = c(p, d, q), seasonal = c(1,0,1))
        #         p_value <- checkresiduals(fit, plot = FALSE)$p.value
        #         forecast_obj <- forecast(fit, h = historical_data)
        #         accuracylist <- as_tibble(accuracy(forecast_obj))
        #         forecast_list <- window(forecast_obj$mean, end = c(year(window_max), month(window_max)))
        #         
        #         comparison <- comparison %>%
        #           mutate(forecast = forecast_list,
        #                  error = (abs(.data[[column]] - forecast) / .data[[column]]) * 100)
        #         
        #         results <- bind_rows(results, tibble(
        #           board = board,
        #           p = p, d = d, q = q,
        #           AIC = AIC(fit),
        #           BIC = BIC(fit),
        #           AICc = fit$aicc,
        #           ME = accuracylist$ME,
        #           MPE = accuracylist$MPE,
        #           MAPE = accuracylist$MAPE,
        #           ACF1 = accuracylist$ACF1,
        #           p_value = p_value,
        #           forecast_error = mean(comparison$error)
        #         ))
        #       }, error = function(e) {
        #         message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
        #       })
        #     }
        #   }
        # }
        
        # SARIMA grid
        p_values <- 0:4
        q_values <- 0:4
        P_values <- 0:1
        Q_values <- 0:1
        
        results <- tibble()
        
        for (p in p_values) {
          for (q in q_values) {
            for (P in P_values) {
              for (Q in Q_values) {
                tryCatch({
                  fit <- Arima(y_ts, order = c(p, 1, q), seasonal = c(P,1,Q))
                  p_value <- checkresiduals(fit, plot = FALSE)$p.value
                  forecast_obj <- forecast(fit, h = historical_data)
                  accuracylist <- as_tibble(accuracy(forecast_obj))
                  forecast_list <- window(forecast_obj$mean, end = c(year(window_max), month(window_max)))
                  
                  comparison <- comparison %>%
                    mutate(forecast = forecast_list,
                           error = (abs(.data[[column]] - forecast) / .data[[column]]) * 100)
                  
                  results <- bind_rows(results, tibble(
                    board = board,
                    p = p, 
                    q = q,
                    P = P,
                    Q = Q,
                    AIC = AIC(fit),
                    BIC = BIC(fit),
                    AICc = fit$aicc,
                    ME = accuracylist$ME,
                    MPE = accuracylist$MPE,
                    MAPE = accuracylist$MAPE,
                    ACF1 = accuracylist$ACF1,
                    p_value = p_value,
                    forecast_error = mean(comparison$error)
                  ))
                }, error = function(e) {
                  message(paste("Error for ARIMA(", p, ",1,", q, ")(", P, ",1,", Q, "):", e$message))
                })
              }
            }
          }
        }
        
        best_result <- results %>% filter(forecast_error == min(forecast_error))
        yearly_results <- bind_rows(yearly_results, best_result)
      }
      
      column_results <- bind_rows(column_results, yearly_results %>% mutate(year = year, max_date = window_max))
    }
    
    full_results <- bind_rows(full_results, column_results %>% mutate(type = column,
                                                                      run = run_number))
  }
  
  return(full_results)
  
  print(paste('Forecast running time:', Sys.time() - start_time, ' minutes.'))
  
}

# 3.4. Run and save forecast here - controlled by forecast_evaluation logical variable ----
forecast_performance <- run_forecast(columns_to_forecast, # ensure this is only one column at a time
                                     healthboards, # using healthboards variable, can be changed to certain boards
                                     years_to_run) # enter time series limit to be used

latest_forecast_date <- format(Sys.Date())

saveRDS(forecast_performance, glue('{path}/shiny/forecasts/sarima/output/', historical_data,' months/run ', run_number,'/forecast-performance-', latest_forecast_date, '.rds'))

# performance_file <- read_excel('shiny/forecasts/sarima/output/forecast-performance.xlsx') %>%
#   bind_rows(forecast_performance)
# 
# write.xlsx(performance_file, 'shiny/forecasts/sarima/output/forecast-performance.xlsx', overwrite = TRUE)

## 3.5. Create full table of forecasted data with confidence intervals - controlled by forecast_results logical variable ----
forecast_performance <- readRDS(glue('{path}/shiny/forecasts/sarima/output/', historical_data,' months/run ', run_number,'/forecast-performance-', latest_forecast_date, '.rds'))

# Create years vector
years <- unique(forecast_performance$year)
columns <- unique(forecast_performance$type)

num_of_months = 12

combined_forecast <- data.frame()
#boards <- unique(results_list$board)

for (hb in healthboards) {
  
  for (column in columns_to_forecast) {
    
    for (y in years_to_run) {
      
      # Year adjustment
      diff <- year(latest_date) - y
      
      date_adj <- board_data %>%
        filter(`Paid Date` == max(`Paid Date`) - months(12 * diff))
      
      window_max <- unique(date_adj$`Paid Date`)
      MaxDate <- window_max - months(num_of_months)
      
      df <- board_data %>% # code does same as it does in run_forecast code
        filter(`Presc Health Board Name` == hb) %>%
        select(`Paid Date`, column) %>%
        #select(Date, `Claim PD Number of Paid Items`) %>%
        filter(`Paid Date` > '2011-03-31' & `Paid Date` < MaxDate + days(1))
      
      y_ts <- ts(#df$`Claim PD Number of Paid Items`,
        df[[column]],
        start = c(2011,4),
        frequency = 12)
      
      filtered_list <- forecast_performance %>% # use results from run_forecast
        filter(board == hb,
               type == column,
               year == y,
               historical_data == num_of_months)
      
      p <- filtered_list$p # plug in parameters
      q <- filtered_list$q
      P <- filtered_list$P # plug in parameters
      Q <- filtered_list$Q
      
      # Attempt ARIMA fit and forecast
      result <- tryCatch({
        fit <- Arima(y_ts, order = c(p, 1, q), seasonal = c(P, 1, Q))
        forecast_obj <- forecast(fit, h = num_of_months + 36)
        
        # Build the data frame if successful
        data.frame(
          Board = hb,
          Date = 
            seq.Date(
              from = ceiling_date(MaxDate %m+% months(1), unit = "month"),  # start at next month boundary
              by = "month",
              length.out = num_of_months + 36
            ) - days(1)  # subtract 1 day to get last day of month
          ,
          Forecast = as.numeric(forecast_obj$mean),
          Lower_80 = as.numeric(forecast_obj$lower[, 1]),
          Upper_80 = as.numeric(forecast_obj$upper[, 1]),
          Lower_95 = as.numeric(forecast_obj$lower[, 2]),
          Upper_95 = as.numeric(forecast_obj$upper[, 2]),
          Type = column,
          Year = y,
          Historical_Data = paste0(num_of_months, ' months of historical data'),
          F_Horizon = num_of_months + 36,
          Arima_Error = FALSE  # No error
        )
      }, error = function(e) {
        message(paste("ARIMA error for", hb, ":", e$message))
        
        # Create placeholder rows with NA values
        data.frame(
          Board = hb,
          Date = seq.Date(Sys.Date(), by = "month", length.out = num_of_months),
          Forecast = NA,
          Lower_80 = NA,
          Upper_80 = NA,
          Lower_95 = NA,
          Upper_95 = NA,
          Type = column,
          Year = y,
          Historical_Data = paste0(num_of_months, ' months of historical data'),
          F_Horizon = num_of_months + 36,
          Arima_Error = TRUE  # Error occurred
        )
      })
      
      # Append to combined_forecast
      combined_forecast <- combined_forecast %>% rbind(result)
      
    }
    
  }
  
}

## 3.6. Create a Scotland total based on the forecasted values from each board, join to original table
scotland_agg_forecast <- combined_forecast %>%
  filter(!(Board == 'SCOTLAND')) %>%
  group_by(Date, Type, Year, Historical_Data, F_Horizon, Arima_Error) %>%
  reframe(Forecast = case_when(Type %in% c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB') ~ sum(Forecast),
                               Type == 'Cost per item' ~ mean(Forecast)),
          Lower_80 = case_when(Type %in% c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB') ~ sum(Lower_80),
                               Type == 'Cost per item' ~ mean(Lower_80)),
          Upper_80 = case_when(Type %in% c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB') ~ sum(Upper_80),
                               Type == 'Cost per item' ~ mean(Upper_80)),
          Lower_95 = case_when(Type %in% c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB') ~ sum(Lower_95),
                               Type == 'Cost per item' ~ mean(Lower_95)),
          Upper_95 = case_when(Type %in% c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB') ~ sum(Upper_95),
                               Type == 'Cost per item' ~ mean(Upper_95))) %>%
  mutate(Board = 'SCOTLAND') %>%
  unique() %>%
  select(Board, Date, Forecast, Lower_80, Upper_80, Lower_95, Upper_95, Type, Year, Historical_Data, F_Horizon, Arima_Error)

# Re-join with forecast
combined_forecast <- combined_forecast %>%
  # This will need to be changed
  filter(!(Board == 'SCOTLAND')) %>%
  rbind(scotland_agg_forecast)

# Create dataframe with each combination of variables to join to observed data (will allow for plotting)
combo <- combined_forecast %>%
  select(Board, Type, Year, Historical_Data, F_Horizon, Arima_Error) %>%
  unique()

result <- board_data %>%
  pivot_longer(cols = c('Claim PD Number of Paid Items', 'Claim PD Paid GIC excl. BB', 'Cost per item'),
               names_to = 'Type',
               values_to = 'Measure') %>% # reframing data to fit what the forecast dataframe looks like
  dplyr::rename(Board = `Presc Health Board Name`,
                Date = `Paid Date`) %>%
  # join with variable combination df to create an instance of each combo for each date
  left_join(combo) %>%
  # now join with the forecast
  full_join(combined_forecast)

# Save final table
saveRDS(result, glue('{path}/shiny/forecasts/sarima/output/forecast-run-', run_number, '.rds'))

# Measuring time
end <- Sys.time()
diff <- end - start

saveRDS(diff, 'shiny/forecasts/sarima/time.rds')
