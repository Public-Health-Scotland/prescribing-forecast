### SARIMA Forecast
### Honours Project
### Date last updated: 03/07/2026
### Author: Liam Rooney
### Model: v2.2

### Changes made to previous model version (v2.1):
### 1. Parameter grid search updated
###    - fixed differencing parameters equal to 1 (d/D)
###      -  Reduces computation time, model complexity whilst increasing model stability

### Key notes for running this script:
### Once the variables at lines 52, 67, 68, 69, and 71 have been defined by the user: 
### 1. Click Source --> Source as Workbench Job --> define the memory parameters (1 CPU/8 GB should suffice)
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

source("functions/core_functions.R")

# These are the columns that will be forecasted
# WARNING: The more columns included, the longer the runtime will be. It is recommended that this script is set to run as a Workbench job.
working_day = FALSE

# Comment columns out as required
columns_to_forecast = c(
  'Claim PD Number of Paid Items',
  'Claim PD Paid GIC excl. BB',
  'Cost per item'
)

# Set this year based on the last month loaded into nDCVP - if you want to run a forecast up until the same month 
# recently loaded into nDCVP but from a different year, you can enter it here too.
years_to_run <- c(2026)

# number of months for forecast to be compared with against historical data (if set at 12, forecast vs. 
# observed for previous year will give an average absolute error)
historical_data = 12

run_number = 5 # update each time
version_number = 2.2 # if new version is created, new script is created and number updated

# List of healthboards, comment out boards if you want to run a forecast for specific boards
healthboards <- c("NHS AYRSHIRE & ARRAN", "NHS BORDERS", "NHS DUMFRIES & GALLOWAY", "NHS FIFE",
                  "NHS FORTH VALLEY", "NHS GRAMPIAN", "NHS GREATER GLASGOW & CLYDE", "NHS HIGHLAND", 
                  "NHS LANARKSHIRE", "NHS LOTHIAN", "NHS TAYSIDE", "NHS WESTERN ISLES", "NHS ORKNEY", 
                  "NHS SHETLAND", "SCOTLAND")

# 2. Read in data and some processing ----
data <- read.csv('data/Time-Series Data/Historical Data.csv', check.names = FALSE) %>% # REMINDER: check that this is the correct file from the BOXI run you have just scheduled
  filter(`Presc Health Board Name` %in% healthboards)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
#data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
#data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values

board_data <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions

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

board_data$`Cost per item`[board_data$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign 0 to any blank values

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

## 3.2. SARIMA Forecast (function) ---- 

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
          select(all_of(`Paid Date`, column)) %>% # can only select date column and predictor variable for use in time series
          filter(`Paid Date` > '2011-03-31' & `Paid Date` < MaxDate + days(1))
        
        y_ts <- ts(df[[column]], start = c(2011,4), frequency = 12) # start of time series limited to post-2011 due to introduction of free prescriptions, frequency is 12 to indicate monthly time-series
        
        comparison <- board_data %>%
          filter(`Presc Health Board Name` == board) %>%
          select(all_of(`Paid Date`, column)) %>%
          filter(`Paid Date` > MaxDate & `Paid Date` < window_max + days(1))
        
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
                                                                      run = run_number,
                                                                      version = version_number))
  }
  
  return(full_results)
  
  print(paste('Forecast running time:', Sys.time() - start_time, ' minutes.'))
  
}

## 3.3. Run and save forecast here - controlled by forecast_evaluation logical variable ----
forecast_performance <- run_forecast(columns_to_forecast, # defined at the top of the script
                                     healthboards, 
                                     years_to_run)

latest_forecast_date <- format(Sys.Date())

saveRDS(forecast_performance, glue('{path}/forecasts/sarima/output/run {run_number}/performance/forecast-performance-{run_number}.rds'))

## 3.4. Update master performance file for use in dashboard (this script contains latest run number) ----
master_pt <- read_excel(glue('{path}/forecasts/sarima/output/forecast-performance.xlsx'))

new_performance <- update_mpt(master_pt, forecast_performance)

write.xlsx(new_performance, glue('{path}/forecasts/sarima/output/forecast-performance.xlsx'), overwrite = TRUE)

## 3.5. Create full table of forecasted data with confidence intervals - controlled by forecast_results logical variable ----
forecast_performance <- readRDS(glue('{path}/forecasts/sarima/output/run {run_number}/performance/forecast-performance-{run_number}.rds'))

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
        select(all_of(`Paid Date`, column)) %>%
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
saveRDS(result, glue('{path}/forecasts/sarima/output/run {run_number}/forecast-run-{run_number}.rds'))

# Measuring time
end <- Sys.time()
diff <- end - start
diff
