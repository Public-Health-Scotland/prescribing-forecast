# ### Data preparation file for showing forecast in dashboard
# ### Honours Project
# ### Date created: 01/08/2025
# ### Author: Liam Rooney
# 
# start_time <- Sys.time()
# 
# # 1. Load packages, key variables and data ----
# options(scipen = 999)
# ## 1.1. Load packages ----
# library(tidyverse)
# library(lubridate)
# library(forecast)
# library(ggplot2)
# library(plotly)
# library(openxlsx)
# library(readxl)
# library(officer)
# library(glue)
# library(devtools)
# library(urca)
# library(phsmethods)
# library(tibble)
# library(bizdays)
# 
# ## 1.2. Initialise variables and file paths ----
# '%!in%' <- function(x,y)!('%in%'(x,y))
# 
# path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'
# 
# ## 1.3. Load in data ----
# 
# # 2. Read in data and some processing
# data <- read.csv('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/BNF Demographic Data.csv', check.names = FALSE) %>%
#   select(-`Paid BNF Chapter Code`)
# 
# data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
# data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
# data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values
# 
# bnf_demographic <- data %>%
#   mutate(`Paid Date` = dmy(`Paid Date`)) %>%
#   filter(`Paid Date` > '2009-12-31') %>% # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions
#   arrange(factor(`Age Band (patient age at paid date)`, levels = c('0-4', '5-9', '10-14', '15-19', '20-24',
#                                                                    '25-29', '30-34', '35-39', '40-44', '45-49',
#                                                                    '50-54', '55-59', '60-64', '65-69', '70-74',
#                                                                    '75-79', '80-84', '85-89', '90+', 'BLANK AGE BAND'))) %>% # arrange age bands in order
#   arrange(factor(`Paid BNF Chapter Description`)) %>%
#   # filter data for board take out chapter
#   filter(`Disp Health Board Name` == 'NHS AYRSHIRE & ARRAN') %>%
#   # group_by(`Paid Date`) %>%
#   # summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#   #           `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
#   # ungroup() %>%
#   mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) #%>%
# # create lagged values
# # group_by(`Age Band (patient age at paid date)`, `Paid BNF Chapter Description`) %>%
# # arrange(`Paid Date`) %>%
# # mutate(lag_1 = lag(`Claim PD Paid GIC excl. BB`, 1),
# #        lag_12 = lag(`Claim PD Paid GIC excl. BB`, 12),
# #        month = lubridate::month(`Paid Date`),   # seasonal feature
# #        year = lubridate::year(`Paid Date`)      # trend feature
# # ) %>%
# # ungroup() %>%
# # filter(!is.na(lag_12)) # take out 2011 data, imputing 0 would 
# 
# bnf_demographic$`Cost per item`[bnf_demographic$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values
# 
# 
# # scotland_data <- rbind(
# #   read_excel(glue('{path}/data/scotland/Scotland_Phasings_04_08.xlsx')),
# #   read_excel(glue('{path}/data/scotland/Scotland_Phasings_09_13.xlsx')),
# #   read_excel(glue('{path}/data/scotland/Scotland_Phasings_14_18.xlsx')),
# #   read_excel(glue('{path}/data/scotland/Scotland_Phasings_19_24.xlsx'))
# # ) %>%
# #   #mutate(Date = ceiling_date(as.Date(paste('1', `Paid Financial Month`, `Paid Financial Year`), format = '%d %m %Y'), unit = 'month') - days(1)) %>%
# #   mutate(Date = as.Date(`Paid Date`),
# #          Board = 'SCOTLAND') %>%
# #   #left_join(hb_code, by = 'Disp Health Board Code') %>%
# #   mutate(FY = extract_fin_year(Date)) %>%
# #   select(Board, Date, FY, `Claim PD Number of Paid Items`, `Claim PD Paid GIC excl. BB`, Phasings)
# 
# healthboards <- unique(bnf_demographic$Board) 
# healthboards <- healthboards[healthboards %!in% c('ARGYLL & CLYDE HEALTH BOARD', 'DUMMY SCOTLAND HB')]
# 
# dates <- unique(hb_data$Date)
# 
# # Forecasting history file
# history <- read_excel(glue('{path}/accuracy/forecasting_history.xlsx')) %>%
#   pivot_longer(cols = 3:ncol(.),
#                names_to = 'Date',
#                values_to = 'value') %>%
#   pivot_wider(names_from = 'Indicator',
#               values_from = 'value') %>%
#   mutate(Date = ceiling_date(as.Date(as.numeric(Date), origin = "1899-12-30"), unit = 'month') - days(1)) %>%
#   left_join(hb_code %>% rename(Board = `Disp Health Board Code`), by = 'Board') %>%
#   filter(!(is.na(`Disp Health Board Name`))) %>%
#   select(-Board) %>%
#   rename(Board = `Disp Health Board Name`) %>%
#   mutate(FY = extract_fin_year(Date)) %>%
#   select(Board, FY, everything())
# 
# # 2. Run forecast ----
# 
# ## 2.1. Initialise tables and variables ----
# 
# latest_date <- max(hb_data$Date)
# max_date <- max(hb_data$Date) - months(12)
# 
# ## ---- only uncomment if running the forecast again ----
# 
# ## Calculate business days for each month, keep commented out if don't want per working day figure
# dates <- unique(hb_data$Date)
# 
# start_date <- min(dates)
# end_date <- as.Date('2026-12-28')
# 
# holidays <- c(
#   # 2004 holidays
#   '2004-01-01', '2004-01-02', '2004-04-09', '2004-05-03', '2004-05-31', '2004-08-02', '2004-11-30', '2004-12-27', '2004-12-28',
#   # 2005 holidays
#   '2005-01-03', '2005-01-04', '2005-03-25', '2005-05-02', '2005-05-30', '2005-08-01', '2005-11-30', '2005-12-26', '2005-12-27',
#   # 2006 holidays
#   '2006-01-02', '2006-01-03', '2006-04-14', '2006-05-01', '2006-05-29', '2006-08-07', '2006-11-30', '2006-12-25', '2006-12-26',
#   # 2007 holidays
#   '2007-01-01', '2007-01-02', '2007-04-06', '2007-05-07', '2007-05-28', '2007-08-06', '2007-11-30', '2007-12-25', '2007-12-26',
#   # 2008 holidays
#   '2008-01-01', '2008-01-02', '2008-03-21', '2008-05-05', '2008-05-26', '2008-08-04', '2008-12-01', '2008-12-25', '2008-12-26',
#   # 2009 holidays
#   '2009-01-01', '2009-01-02', '2009-04-10', '2009-05-04', '2009-05-25', '2009-08-03', '2009-11-30', '2009-12-25', '2009-12-28',
#   # 2010 holidays
#   '2010-01-01', '2010-01-04', '2010-04-02', '2010-05-03', '2010-05-31', '2010-08-02', '2010-11-30', '2010-12-27', '2010-12-28',
#   # 2011 holidays
#   '2011-01-03', '2011-01-04', '2011-04-22', '2011-04-29', '2011-05-02', '2011-05-30', '2011-08-01', '2011-11-30', '2011-12-26', '2011-12-27',
#   # 2012 holidays
#   '2012-01-02', '2012-01-03', '2012-04-06', '2012-05-07', '2012-06-04', '2012-06-05', '2012-08-06', '2012-11-30', '2012-12-25', '2012-12-26',
#   # 2013 holidays
#   '2013-01-01', '2013-01-02', '2013-03-29', '2013-05-06', '2013-05-27', '2013-08-05', '2013-12-02', '2013-12-25', '2013-12-26',
#   # 2014 holidays
#   '2014-01-01', '2014-01-02', '2014-04-18', '2014-05-05', '2014-05-26', '2014-08-04', '2014-12-01', '2014-12-25', '2014-12-26',
#   # 2015 holidays
#   '2015-01-01', '2015-01-02', '2015-04-03', '2015-05-04', '2015-05-25', '2015-08-03', '2015-11-30', '2015-12-25', '2015-12-28',
#   # 2016 holidays
#   '2016-01-01', '2016-01-04', '2016-03-25', '2016-05-02', '2016-05-30', '2016-08-01', '2016-11-30', '2016-12-26', '2016-12-27',
#   # 2017 holidays
#   '2017-01-02', '2017-01-03', '2017-04-14', '2017-05-01', '2017-05-29', '2017-08-07', '2017-11-30', '2017-12-25', '2017-12-26',
#   # 2018 holidays
#   '2018-01-01', '2018-01-02', '2018-03-30', '2018-05-07', '2018-05-28', '2018-08-06', '2018-11-30', '2018-12-25', '2018-12-26',
#   # 2019 holidays
#   '2019-01-01', '2019-01-02', '2019-04-19', '2019-05-06', '2019-05-27', '2019-08-05', '2019-12-02', '2019-12-25', '2019-12-26',
#   # 2020 holidays
#   '2020-01-01', '2020-01-02', '2020-04-10', '2020-05-08', '2020-05-25', '2020-08-03', '2020-11-30', '2020-12-25', '2020-12-28',
#   # 2021 holidays
#   '2021-01-01', '2021-01-04', '2021-04-02', '2021-05-03', '2021-05-31', '2021-08-02', '2021-11-30', '2021-12-27', '2021-12-28',
#   # 2022 holidays
#   '2022-01-03', '2022-01-04', '2022-04-15', '2022-05-02', '2022-06-02', '2022-06-03', '2022-08-01', '2022-09-19', '2022-11-30', '2022-12-26', '2022-12-27',
#   # 2023 holidays
#   '2023-01-02', '2023-01-03', '2023-04-07', '2023-05-01', '2023-05-08', '2023-05-29', '2023-08-07', '2023-11-30', '2023-12-25', '2023-12-26',
#   # 2024 holidays
#   '2024-01-01', '2024-01-02', '2024-03-29', '2024-05-06', '2024-05-27', '2024-08-05', '2024-12-02', '2024-12-25', '2024-12-26',
#   # 2025 holidays
#   '2025-01-01', '2025-01-02', '2025-04-18', '2025-05-05', '2025-05-26', '2025-08-04', '2025-12-01', '2025-12-25', '2025-12-26',
#   # 2026 holidays
#   '2026-01-01', '2026-01-02', '2026-04-03', '2026-05-04', '2026-05-25', '2026-08-03', '2026-11-30', '2026-12-25', '2026-12-28',
#   "2027-01-01"
#   # add in holidays for further years below
#   
#   
# )
# 
# #saveRDS(holidays, '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/data/holidays.rds')
# 
# # Create a calendar excluding weekends and holidays
# Scotland <- create.calendar(name = "Scotland", weekdays = c("saturday", "sunday"), holidays = holidays)
# 
# # Generate a sequence of months
# months <- seq(
#   from = as.Date(format(start_date, "%Y-%m-01")),
#   to   = as.Date(format(end_date, "%Y-%m-01")),
#   by   = "month"
# )
# 
# # Calculate business days for each month
# business_days <- sapply(months, function(month) {
#   first_day <- as.Date(format(month, "%Y-%m-01"))
#   last_day  <- ceiling_date(first_day, "month") - days(1)
#   bizdays::bizdays(first_day, last_day, Scotland)
# })
# 
# # Combine results into a data frame
# result <- data.frame(
#   Date = as.Date(format(ceiling_date(months, "month") - days(1), "%Y-%m-%d")),
#   Business_Days = business_days
# )
# 
# saveRDS(result, "/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/business_days_lookup.rds")
# 
# #result <- read_rds(glue("{path}/shiny/data/business_days_lookup.rds"))
# 
# 
# start_time <- Sys.time()
# 
# results_list <- tibble(board = character(),
#                        p = numeric(),
#                        d = numeric(),
#                        q = numeric(),
#                        AIC = double(),
#                        BIC = double(),
#                        AICc = double(),
#                        ME = double(),
#                        MPE = double(),
#                        MAPE = double(),
#                        ACF1 = double(),
#                        p_value = double(),
#                        forecast_error = double())
# 
# new_hb_data <- hb_data %>% # comment code below out depending on forecast
#   left_join(result, by = 'Date') %>%
#   mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
#   filter(Board == 'NHS AYRSHIRE & ARRAN',
#          Date > '2011-03-31')
# 
# max_year <- max(year(new_hb_data$Date))
# 
# 
# ## 5. SARIMA forecast
# bnf_chapters <- unique(bnf_demographic$`Paid BNF Chapter Description`)
# age_bands <- unique(bnf_demographic$`Age Band (patient age at paid date)`)
# 
# mapped_age_chap <- expand.grid(
#   BNF_Chapter = bnf_chapters,
#   AgeBand = age_bands,
#   stringsAsFactors = FALSE
# )
# 
# months <- seq(as.Date("2025-09-30"), as.Date("2026-08-31"), by = "month")
# 
# future_grid <- merge(mapped_age_chap, data.frame(month = months), all = TRUE)
# future_grid$year <- year(future_grid$month)
# future_grid$month_num <- month(future_grid$month)
# 
# library(forecast)
# 
# sarima_forecasts <- list()
# 
# for (i in seq_len(nrow(mapped_age_chap))) {
#   chapter <- mapped_age_chap$BNF_Chapter[i]
#   age <- mapped_age_chap$AgeBand[i]
#   
#   # Filter historical data for this pair
#   ts_data <- bnf_demographic %>%
#     filter(`Paid BNF Chapter Description` == chapter,
#            `Age Band (patient age at paid date)` == age) %>%
#     arrange(`Paid Date`)
#   
#   # Fit SARIMA for Paid Items
#   ts_paid <- ts(ts_data$`Claim PD Number of Paid Items`, frequency = 12)
#   fit_paid <- auto.arima(ts_paid, seasonal = TRUE)
#   forecast_paid <- forecast(fit_paid, h = 12)$mean
#   
#   # Fit SARIMA for Cost per item
#   ts_cost <- ts(ts_data$`Cost per item`, frequency = 12)
#   fit_cost <- auto.arima(ts_cost, seasonal = TRUE)
#   forecast_cost <- forecast(fit_cost, h = 12)$mean
#   
#   # Store forecasts
#   sarima_forecasts[[paste(chapter, age, sep = "_")]] <- data.frame(
#     Paid_BNF_Chapter_Description = chapter,
#     Age_Band = age,
#     month = months,
#     year = year(months),
#     Claim_PD_Number_of_Paid_Items = forecast_paid,
#     Cost_per_item = forecast_cost
#   )
# }
# 
# saveRDS(sarima_forecasts, '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/sarima_forecasts.rds')
# 
# # Convert all ts columns to numeric in each data frame
# test <- lapply(sarima_forecasts, function(df) {
#   df[] <- lapply(df, function(col) {
#     if (inherits(col, "ts")) as.numeric(col) else col
#   })
#   return(df)
# })
# 
# # Combine into one data frame
# future_data <- do.call(rbind, test)
# 
# 
# future_data <- do.call(rbind, sarima_forecasts)
# 
# 
# ## Forecast
# bnf_demographic <- read.csv('data/BNF Demographic Data.csv', check.names = FALSE) %>%
#   select(-`Paid BNF Chapter Code`)
# 
# bnf_demographic$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", bnf_demographic$`Claim PD Paid GIC excl. BB`))
# bnf_demographic$`Paid BNF Chapter Description`[bnf_demographic$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
# bnf_demographic$`Age Band (patient age at paid date)`[bnf_demographic$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values
# 
# bnf_demographic <- bnf_demographic %>%
#   mutate(`Paid Date` = dmy(`Paid Date`)) %>%
#   group_by(`Disp Health Board Name`, `Paid Date`) %>%
#   summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#             `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
#   ungroup() %>%
#   mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
#   select(-`Claim PD Number of Paid Items`, -`Cost per item`)
#   #select(-`Claim PD Paid GIC excl. BB`)
# 
# max_year <- max(year(bnf_demographic$`Paid Date`))
# 
# 
# run_forecast <- function(columns, hbs, years) {
#   
#   start_time <- Sys.time()
#   
#   full_results <- tibble()  # Initialise cleanly
#   
#   for (column in columns) {
#     
#     column_results <- tibble()
#     
#     for (year in years) {
#       
#       yearly_results <- tibble()
#       
#       # Year adjustment
#       y <- ifelse(year <= max_year, year, max_year)
#       diff <- year(Sys.time()) - y
#       
#       MaxDate <- bnf_demographic %>%
#         filter(`Paid Date` == max(`Paid Date`) - months(12 * diff))
#       
#       window_max <- unique(MaxDate$`Paid Date`)
#       MaxDate <- window_max - months(12)
#       
#       for (board in hbs) {
#         
#         df <- bnf_demographic %>%
#           filter(`Disp Health Board Name` == board) %>%
#           select(`Paid Date`, column) %>% # can only select date column and predictor variable for use in time series
#           filter(`Paid Date` > '2010-12-31' & `Paid Date` < MaxDate + days(1)) # start of time series limited to post-2011 due to introduction of free prescriptions
#         
#         y_ts <- ts(df[[column]], start = 2011, frequency = 12) # start of time series limited to post-2011 due to introduction of free prescriptions
#         
#         comparison <- bnf_demographic %>%
#           filter(`Disp Health Board Name` == board) %>%
#           select(`Paid Date`, column) %>%
#           filter(`Paid Date` > MaxDate & `Paid Date` < window_max + days(1))
#         
#         # ARIMA grid
#         p_values <- 0:9
#         d_values <- 0:1
#         q_values <- 0:9
#         
#         results <- tibble()
#         
#         for (p in p_values) {
#           for (d in d_values) {
#             for (q in q_values) {
#               tryCatch({
#                 fit <- Arima(y_ts, order = c(p, d, q), seasonal = c(1,0,1))
#                 p_value <- checkresiduals(fit, plot = FALSE)$p.value
#                 forecast_obj <- forecast(fit, h = 24)
#                 accuracylist <- as_tibble(accuracy(forecast_obj))
#                 forecast_list <- window(forecast_obj$mean, end = c(year(window_max), month(window_max)))
#                 
#                 comparison <- comparison %>%
#                   mutate(forecast = forecast_list,
#                          error = (abs(forecast - .data[[column]]) / forecast) * 100)
#                 
#                 results <- bind_rows(results, tibble(
#                   board = board,
#                   p = p, d = d, q = q,
#                   AIC = AIC(fit),
#                   BIC = BIC(fit),
#                   AICc = fit$aicc,
#                   ME = accuracylist$ME,
#                   MPE = accuracylist$MPE,
#                   MAPE = accuracylist$MAPE,
#                   ACF1 = accuracylist$ACF1,
#                   p_value = p_value,
#                   forecast_error = mean(comparison$error)
#                 ))
#               }, error = function(e) {
#                 message(paste("Error for ARIMA(", p, ",", d, ",", q, "):", e$message))
#               })
#             }
#           }
#         }
#         
#         best_result <- results %>% filter(forecast_error == min(forecast_error))
#         yearly_results <- bind_rows(yearly_results, best_result)
#       }
#       
#       column_results <- bind_rows(column_results, yearly_results %>% mutate(year = year))
#     }
#     
#     full_results <- bind_rows(full_results, column_results %>% mutate(type = column))
#   }
#   
#   return(full_results)
#   
#   print(paste('Forecast running time:', Sys.time() - start_time, ' minutes.'))
#   
# }
# 
# ## 2.3. Run forecasts here ----
# forecast <- run_forecast(c('Claim PD Paid GIC excl. BB'),
#                          'NHS AYRSHIRE & ARRAN', # using healthboards variable, can be changed to certain boards
#                          max_year) # enter time series limit to be used (2023 will )
# 
# latest_forecast_date <- format(Sys.Date()) # change this to date of last forecast
# saveRDS(forecast, paste0('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/forecast-', latest_forecast_date, '-wd.rds'))
# 
# results_list <- readRDS(paste0('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/forecast-', latest_forecast_date, '-wd.rds')) # change depending on date of forecast run
# 
# final_forecast <- data.frame()
# rmse_sarima <- data.frame()
# 
# boards <- unique(results_list$board)
# 
# max_date <- max(bnf_demographic$`Paid Date`) - months(12)
# 
# last_date <- floor_date(max(bnf_demographic$`Paid Date`), "month")
# future_dates <- ceiling_date(seq(from = last_date %m+% months(1), by = "month", length.out = 48), "month") - days(1)
# 
# column <- 'Cost per item'
# 
# for (column in c('Claim PD Paid GIC excl. BB')) {
#   
#   combined_forecast <- data.frame()
#   comparison <- data.frame()
#   
#   for (x in boards) {
#     
#     df <- bnf_demographic %>% # code does same as it does in run_forecast code
#       filter(`Disp Health Board Name` == x) %>%
#       select(`Paid Date`, column) %>%
#       #select(`Paid Date`, `Claim PD Number of Paid Items`) %>%
#       filter(`Paid Date` > '2010-12-31' & `Paid Date` < max_date + days(1))
#     
#     #dates <- unique(df$`Paid Date`)
#     
#     y <- ts(#df$`Claim PD Number of Paid Items`,
#       df[[column]],
#       start = 2011,
#       frequency = 12)
#     
#     filtered_list <- results_list %>% # use results from run_forecast
#       filter(board == x,
#              type == column)
#     
#     comparison <- comparison %>%
#       rbind(
#         bnf_demographic %>%
#           filter(`Disp Health Board Name` == x) %>%
#           select(`Paid Date`, column) %>%
#           filter(`Paid Date` > max_date & `Paid Date` < as.Date('2025-08-31') + days(1))
#       )
#     
#     p <- filtered_list$p # plug in parameters
#     d <- filtered_list$d
#     q <- filtered_list$q
#     
#     fit <- Arima(y, order = c(p, d, q), seasonal = c(1,0,1))
#     
#     forecast <- forecast(fit, h = 48)
# 
#     forecast_list <- window(forecast$mean, end = c(2025, 8))
#     
#     comparison <- comparison %>%
#       mutate(forecast = as.numeric(forecast_list),
#              error = as.numeric((abs(forecast - .data[[column]]) / forecast)) * 100,
#              #rmse = sqrt(mean(abs(column - forecast) * 2)),
#              type = column) %>%
#       dplyr::rename(observed = column)
#     
#     comparison <- comparison %>%
#       mutate(rmse = sqrt(mean(abs(observed - forecast) * 2)))
#     
#     combined_forecast <- combined_forecast %>%
#       rbind(data.frame(
#         Board = x,
#         Date = future_dates,
#         Forecast = as.numeric(forecast$mean),
#         Lower_80 = as.numeric(forecast$lower[, 1]),
#         Upper_80 = as.numeric(forecast$upper[, 1]),
#         Lower_95 = as.numeric(forecast$lower[, 2]),
#         Upper_95 = as.numeric(forecast$upper[, 2]),
#         Type = column
#       ))
#     
#   }
#   
#   final_forecast <- final_forecast %>%
#     rbind(combined_forecast)
#   
#   rmse_sarima <- rmse_sarima %>%
#     rbind(comparison)  
# }
# 
# #saveRDS(rmse_sarima, 'rmse_sarima.rds')
# saveRDS(rmse_sarima, 'rmse_sarima_gic.rds')
# 
# #predictions <- forecast$mean
# 
# final_df <- final_forecast %>%
#   select(Board, Date, Forecast, Type) %>%
#   pivot_wider(names_from = Type,
#               values_from = Forecast) %>%
#   dplyr::rename(`Disp Health Board Name` = Board,
#                 `Paid Date` = Date) %>%
#   rbind(bnf_demographic %>% filter(`Disp Health Board Name` == 'NHS AYRSHIRE & ARRAN')) %>%
#   arrange(`Paid Date`)
# 
# 
# #saveRDS(final_df, '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/sarima_future_forecast.rds')
# 
# saveRDS(final_df, '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/sarima_future_forecast_gic.rds')
# 
# 
# 
# 
# 
# 
# 
# 
