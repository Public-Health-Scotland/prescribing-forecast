### SARIMA Forecast
### Honours Project
### Date last updated: 15/01/2026
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

## 1.3. Logical variables to assist in job run
working_day = FALSE
forecast_evaluation = FALSE
forecast_results = TRUE

# These are the columns that will be forecasted
# WARNING: The more columns included, the longer the runtime will be. It is recommended that this script is set to run as a Workbench job.

# Comment columns out as required
columns_to_forecast = c(
  #'Claim PD Number of Paid Items'#,
  #'Claim PD Paid GIC excl. BB'#,
  'Cost per item'
)

# This variable decides what years to cap the time series at upon each run
# Time series begins in 2011, we need 75% of time series to be training data, and 25% to be test data

# Month of the year that time series is capped on will be guided by the last month of data loaded into PIS

# For example, running the script to forecast for the four years following 2023 would result in the time series 
# being capped at August 2020 if that was the last month of data loaded into PIS at the time of running. This ensures
# that roughly 25% of the time series is included in the test data and that said data covers a whole number of years (3 in this example).
years_to_run <- c(2025)

historical_data = 12 # months

run_number = 3

# 2. Read in data and some processing ----
data <- read.csv(paste0(glue('shiny/forecasts/data/Historical Data.csv')), check.names = FALSE)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
#data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
#data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values

board_data <- data %>%
  #bnf_demographic <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') #%>% # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions
# aggregate data to exclude BNF chapter and age band - comment out if wanting full dataframe
# group_by(`Disp Health Board Name`, `Paid Date`) %>%
# summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#           `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
# ungroup()

rm(data)

## 2.1. Aggregate and join Scotland data ----
scotland_data <- board_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(`Disp Health Board Name` = 'SCOTLAND') %>%
  select(`Disp Health Board Name`, everything())

board_data <- board_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # double check data is arranged by board and date 
  arrange(`Disp Health Board Name`, `Paid Date`)

board_data$`Cost per item`[board_data$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values

## 2.2 Create healthboard and date variables ----
healthboards <- unique(board_data$`Disp Health Board Name`) 
dates <- unique(board_data$`Paid Date`)

# 3. Investigating time-series
data <- board_data %>%
  filter(`Disp Health Board Name` == 'SCOTLAND') %>%
  select(`Paid Date`, `Claim PD Number of Paid Items`)

ts_data <- ts(data$`Claim PD Number of Paid Items`, start = c(2010, 4), frequency = 12)

autoplot(ts_data) +
  labs(title = "Scotland: number of paid prescription items",
       y="Number of Paid Items")

as_tsibble(ts_data) %>%
  gg_tsdisplay(difference(value, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

# 3. Run forecast ----

## 3.1. Initialise key variables and tables to be used ----

latest_date <- max(board_data$`Paid Date`)
max_date <- max(board_data$`Paid Date`) - months(12)

## 3.2. Calculating business days - controlled by working_day logical variable ----

if (working_day == TRUE) {
  
  ## Calculate business days for each month, keep commented out if don't want per working day figure
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
          filter(`Disp Health Board Name` == board) %>%
          select(`Paid Date`, column) %>% # can only select date column and predictor variable for use in time series
          filter(`Paid Date` > '2010-12-31' & `Paid Date` < MaxDate + days(1))
        
        months <- NROW(df$`Paid Date`)
        
        train_months <- round(months * 0.7, 0)
        vali_months <- round(months - train_months, 0)
        #vali_months <- round(months - train_months - test_months, 0)

        train_ts <- df[1:train_months,]
        vali_ts <- df[(train_months+1):(train_months+test_months),]
        #vali_ts <- df[(train_months+test_months+1):months,]
        
        ts_start <- function(df, date_col = "Paid Date") {
          d <- min(df[[date_col]], na.rm = TRUE)
          c(year(d), month(d))
        }
        
        train_ts <- ts(train_ts[[column]], start = ts_start(train_ts), frequency = 12)
        #vali_ts <- ts(vali_ts[[column]], start = ts_start(vali_ts), frequency = 12)
        #vali_ts <- ts(vali_ts[[column]], start = ts_start(vali_ts), frequency = 12)
        
        comparison <- board_data %>%
          filter(`Disp Health Board Name` == board) %>%
          select(`Paid Date`, column) %>%
          filter(`Paid Date` > MaxDate & `Paid Date` < window_max + days(1))
        
        # ARIMA grid
        p_values <- 0:9
        d_values <- 0:1
        q_values <- 0:9
        
        results <- tibble()
        
        for (p in p_values) {
          for (d in d_values) {
            for (q in q_values) {
              tryCatch({
                fit <- Arima(y_ts, order = c(p, d, q), seasonal = c(1,0,1))
                p_value <- checkresiduals(fit, plot = FALSE)$p.value
                forecast_obj <- forecast(fit, h = historical_data)
                accuracylist <- as_tibble(accuracy(forecast_obj))
                forecast_list <- window(forecast_obj$mean, end = c(year(window_max), month(window_max)))
                
                comparison <- comparison %>%
                  mutate(forecast = forecast_list,
                         error = (abs(forecast - .data[[column]]) / forecast) * 100)
                
                results <- bind_rows(results, tibble(
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
                ))
              }, error = function(e) {
                message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
              })
            }
          }
        }
        
        best_result <- results %>% filter(forecast_error == min(forecast_error))
        yearly_results <- bind_rows(yearly_results, best_result)
      }
      
      column_results <- bind_rows(column_results, yearly_results %>% mutate(year = year))
    }
    
    full_results <- bind_rows(full_results, column_results %>% mutate(type = column))
  }
  
  return(full_results)
  
  print(paste('Forecast running time:', Sys.time() - start_time, ' minutes.'))
  
}

# 3.4. Run and save forecast here - controlled by forecast_evaluation logical variable ----
if (forecast_evaluation == TRUE) {
  
  forecast <- run_forecast(columns_to_forecast,
                           healthboards, # using healthboards variable, can be changed to certain boards
                           years_to_run) # enter time series limit to be used (2023 will )
  
  latest_forecast_date <- format(Sys.Date()) # change this to date of last forecast
  
  if (unique(forecast$type) == 'Claim PD Number of Paid Items') {
    suffix = 'items'
  } else if (unique(forecast$type) == 'Claim PD Paid GIC excl. BB') {
    suffix = 'gic'
  } else if (unique(forecast$type) == 'Cost per item') {
    suffix = 'cpi'
  }
  
  saveRDS(forecast, paste0(glue('{path}/shiny/forecasts/sarima/output/', historical_data,' months/run ', run_number,'/SARIMA-', latest_forecast_date, '-', suffix, '-', historical_data, 'months.rds')))
  
}


## 3.5. Create full table of forecasted data with confidence intervals - controlled by forecast_results logical variable ----
if (forecast_results == TRUE) {
  
  suffixes = c(
    'items',
    'gic',
    'cpi'
  )
  
  num_of_months = c(
    12#,
    #24,
    #36
  )
  
  # Already defined above but uncomment if needed
  #run_number = 2
  
  results_list <- list()
  
  for (prev_data in num_of_months) {
    for (suffix in suffixes) {
      folder_path <- glue("{path}/shiny/forecasts/sarima/output/{prev_data} months/run {run_number}")
      
      # Find matching file
      file_name <- list.files(folder_path, 
                              pattern = glue("SARIMA-.*-{suffix}-{prev_data}months\\.rds$"), 
                              full.names = TRUE)
      
      if (length(file_name) == 0) {
        warning(glue("No file found for suffix '{suffix}' and prev_data '{prev_data}'"))
        next
      }
      
      # Extract date from filename (YYYY-MM-DD)
      forecast_date <- str_extract(basename(file_name), "\\d{4}-\\d{2}-\\d{2}")
      
      # Read file and add metadata
      df <- readRDS(file_name) %>%
        mutate(
          historical_data = prev_data,
          suffix = suffix,
          forecast_date = forecast_date,
          run = run_number
        )
      
      results_list[[glue("{suffix}_{prev_data}")]] <- df
    }
  }
  
  # Combine all into one dataframe
  combined_results <- bind_rows(results_list)
  
  # Create years vector
  years <- unique(combined_results$year)
  columns <- unique(combined_results$type)
  
  saveRDS(combined_results, glue('{path}/shiny/forecasts/sarima/output/', historical_data,' months/run ', run_number,'/forecast-performance-', max(combined_results$forecast_date), '.rds'))
  
  combined_forecast <- data.frame()
  #boards <- unique(results_list$board)
  
  for (hb in healthboards) {
    
    #board_results <- data.frame()
    
    for (column in columns) {
      
      #column_results <- data.frame()
      
      for (y in years) {
        
        #year_results <- data.frame()
        
        for (prev_data in num_of_months) {
          
          # Year adjustment
          diff <- year(latest_date) - y
          
          date_adj <- board_data %>%
            filter(`Paid Date` == max(`Paid Date`) - months(12 * diff))
          
          window_max <- unique(date_adj$`Paid Date`)
          MaxDate <- window_max - months(prev_data)
          
          df <- board_data %>% # code does same as it does in run_forecast code
            filter(`Disp Health Board Name` == hb) %>%
            select(`Paid Date`, column) %>%
            #select(Date, `Claim PD Number of Paid Items`) %>%
            filter(`Paid Date` > '2010-12-31' & `Paid Date` < MaxDate + days(1))
          
          y_ts <- ts(#df$`Claim PD Number of Paid Items`,
            df[[column]],
            start = 2011,
            frequency = 12)
          
          filtered_list <- combined_results %>% # use results from run_forecast
            filter(board == hb,
                   type == column,
                   year == y,
                   historical_data == prev_data)
          
          p <- filtered_list$p # plug in parameters
          d <- filtered_list$d
          q <- filtered_list$q
          
          # Attempt ARIMA fit and forecast
          result <- tryCatch({
            fit <- Arima(y_ts, order = c(p, d, q), seasonal = c(1, 0, 1))
            forecast_obj <- forecast(fit, h = prev_data + 36)
            
            # Build the data frame if successful
            data.frame(
              Board = hb,
              Date = 
                seq.Date(
                  from = ceiling_date(MaxDate %m+% months(1), unit = "month"),  # start at next month boundary
                  by = "month",
                  length.out = prev_data + 36
                ) - days(1)  # subtract 1 day to get last day of month
              ,
              Forecast = as.numeric(forecast_obj$mean),
              Lower_80 = as.numeric(forecast_obj$lower[, 1]),
              Upper_80 = as.numeric(forecast_obj$upper[, 1]),
              Lower_95 = as.numeric(forecast_obj$lower[, 2]),
              Upper_95 = as.numeric(forecast_obj$upper[, 2]),
              Type = column,
              Year = y,
              Historical_Data = paste0(prev_data, ' months of historical data'),
              F_Horizon = prev_data + 36,
              Arima_Error = FALSE  # No error
            )
          }, error = function(e) {
            message(paste("ARIMA error for", hb, ":", e$message))
            
            # Create placeholder rows with NA values
            data.frame(
              Board = hb,
              Date = seq.Date(Sys.Date(), by = "month", length.out = prev_data),
              Forecast = NA,
              Lower_80 = NA,
              Upper_80 = NA,
              Lower_95 = NA,
              Upper_95 = NA,
              Type = column,
              Year = y,
              Historical_Data = paste0(prev_data, ' months of historical data'),
              F_Horizon = prev_data + 36,
              Arima_Error = TRUE  # Error occurred
            )
          })
          
          # Append to combined_forecast
          combined_forecast <- combined_forecast %>% rbind(result)
          
        }
        
      }
      
    }
    
  }
  
  # saveRDS(combined_forecast, glue('{path}/shiny/forecasts/sarima/output/forecast-', max(combined_results$forecast_date), '.rds'))
  # 
  # combined_forecast <- readRDS(glue('{path}/shiny/forecasts/sarima/output/forecast-2025-11-18.rds'))
  
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
    dplyr::rename(Board = `Disp Health Board Name`,
                  Date = `Paid Date`) %>%
    # join with variable combination df to create an instance of each combo for each date
    left_join(combo) %>%
    # now join with the forecast
    full_join(combined_forecast)
  
  saveRDS(result, glue('{path}/shiny/forecasts/sarima/output/forecast-run-', run_number, '.rds'))
  
}

end <- Sys.time()
diff <- end - start

saveRDS(diff, 'shiny/forecasts/sarima/time.rds')
