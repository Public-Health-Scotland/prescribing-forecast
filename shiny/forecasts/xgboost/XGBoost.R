################## Script to carry out XgBoost predictions ################### 

#### Liam Rooney
#### Honours Project
#### PHS Prescribing Team          
#### Script created 5th November 2025

# 1. Load in packages
library(tidyverse)
library(lubridate)
library(xgboost)
library(forecast)
library(caret)
library(ggplot2)
library(plotly)

# 2. Read in data and some processing
data <- read.csv('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/BNF Demographic Data.csv', check.names = FALSE) %>%
  select(-`Paid BNF Chapter Code`)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values

# Create Scotland data
scotland_data <- data %>%
  group_by(`Paid Date`, `Paid BNF Chapter Description`, `Age Band (patient age at paid date)`) %>%
  summarise(`Disp Health Board Name` = 'SCOTLAND',
         `Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
         `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
  ungroup() %>%
  select(`Disp Health Board Name`, everything())

data <- data %>%
  rbind(scotland_data)

for (board in c('NHS AYRSHIRE & ARRAN', 'NHS LOTHIAN', 'NHS GREATER GLASGOW & CLYDE')) {

bnf_demographic <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > '2009-12-31') %>% # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions
  arrange(factor(`Age Band (patient age at paid date)`, levels = c('0-4', '5-9', '10-14', '15-19', '20-24',
                                                                   '25-29', '30-34', '35-39', '40-44', '45-49',
                                                                   '50-54', '55-59', '60-64', '65-69', '70-74',
                                                                   '75-79', '80-84', '85-89', '90+', 'BLANK AGE BAND'))) %>% # arrange age bands in order
  # filter data for board take out chapter
  filter(`Disp Health Board Name` == board) %>%
  # group_by(`Paid Date`) %>%
  # summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
  #           `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
  # ungroup() %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  # create lagged values
  group_by(`Age Band (patient age at paid date)`, `Paid BNF Chapter Description`) %>%
  arrange(`Paid Date`) %>%
  mutate(lag_1 = lag(`Claim PD Paid GIC excl. BB`, 1),
         lag_12 = lag(`Claim PD Paid GIC excl. BB`, 12),
         month = lubridate::month(`Paid Date`),   # seasonal feature
         year = lubridate::year(`Paid Date`)      # trend feature
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_12)) # take out 2011 data, imputing 0 would 

bnf_demographic$`Cost per item`[bnf_demographic$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values


# 3. Split dataset into train and test data
train <- bnf_demographic %>%
  filter(`Paid Date` < max(`Paid Date`) - months(36) + days(1))

test <- bnf_demographic[-(1:nrow(train)),]

# Define correct order
correct_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+", "BLANK AGE BAND")

bnf_levels <- levels(factor(train$`Paid BNF Chapter Description`))

train$`Age Band (patient age at paid date)` <- factor(train$`Age Band (patient age at paid date)`, levels = correct_order)
train$`Paid BNF Chapter Description` <- factor(train$`Paid BNF Chapter Description`, levels = bnf_levels)

test$`Age Band (patient age at paid date)` <- factor(test$`Age Band (patient age at paid date)`, levels = correct_order)
test$`Paid BNF Chapter Description` <- factor(test$`Paid BNF Chapter Description`, levels = bnf_levels)

## 3.1. Manipulate train and test into matrices
train_matrix <- train %>% select(-`Disp Health Board Name`, -`Paid Date`, -`Claim PD Paid GIC excl. BB`)
test_matrix <- test %>% select(-`Disp Health Board Name`, -`Paid Date`, -`Claim PD Paid GIC excl. BB`)

# One-hot encode everything automatically
train_matrix_numeric <- model.matrix(~ . - 1, data = train_matrix)
test_matrix_numeric <- model.matrix(~ . - 1, data = test_matrix)

# Label must come from corresponding data
train_targets <- train$`Claim PD Paid GIC excl. BB`
test_targets <- test$`Claim PD Paid GIC excl. BB`

dtrain <- xgb.DMatrix(data = train_matrix_numeric, label = train_targets)
dtest <- xgb.DMatrix(data = test_matrix_numeric, label = test_targets)

# 3.3. Define parameters with categorical enabled
params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 3.4. Train model with early stopping
model_xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10000,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 20,
  verbose = 1
)

saveRDS(model_xgb, paste0('shiny/forecasts/xgboost/model_xgb_', board, '.rds'))

}

#'NHS AYRSHIRE & ARRAN', 'NHS GREATER GLASGOW & CLYDE', 'NHS LOTHIAN', 'NHS TAYSIDE', 'NHS HIGHLAND'

aa_xgb <- readRDS('shiny/forecasts/xgboost/model_xgb_NHS AYRSHIRE & ARRAN.rds')
ggc_xgb <- readRDS('shiny/forecasts/xgboost/model_xgb_NHS GREATER GLASGOW & CLYDE.rds')
lothian_xgb <- readRDS('shiny/forecasts/xgboost/model_xgb_NHS LOTHIAN.rds')
tayside_xgb <- readRDS('shiny/forecasts/xgboost/model_xgb_NHS TAYSIDE.rds')
highland_xgb <- readRDS('shiny/forecasts/xgboost/model_xgb_NHS HIGHLAND.rds')


models <- list(
  aa_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_NHS AYRSHIRE & ARRAN.rds'),
  ggc_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_NHS GREATER GLASGOW & CLYDE.rds'),
  lothian_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_NHS LOTHIAN.rds'),
  tayside_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_NHS TAYSIDE.rds'),
  highland_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_NHS HIGHLAND.rds'),
  scotland_xgb = readRDS('shiny/forecasts/xgboost/model_xgb_SCOTLAND.rds')
)


for (name in names(models)) {
  model <- models[[name]]
  
  # Compute importance
  feature_names <- colnames(model$feature_names)
  importance_matrix <- xgb.importance(feature_names = feature_names, model = model)
  
  # Create ggplot object
  plot_obj <- xgb.plot.importance(importance_matrix, top_n = 10) 
  
  plot <- ggplot(plot_obj, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste0("Feature Importance for ", name)) +
    theme_minimal()
  
  plot
  
  # Save plot
  ggsave(
    filename = paste0('shiny/forecasts/xgboost/', name, '_importance.png'),
    plot = plot,
    width = 8,
    height = 6
  )
  
}


# 
# library(ggplot2)
# 
# 
# #saveRDS(model_xgb, 'model_xgb.rds')
# model_xgb <- readRDS('model_xgb.rds')
# 
# eval_log <- model_xgb$evaluation_log
# ggplot(eval_log, aes(x = iter)) +
#   geom_line(aes(y = train_rmse, colour = "Train")) +
#   geom_line(aes(y = eval_rmse, colour = "Eval")) +
#   labs(y = "RMSE", x = "Iteration", title = "Training vs Validation RMSE")
# 
# 
# 
# # 4. Predictions
# pred_train <- predict(model_xgb, dtrain)
# pred_test <- predict(model_xgb, dtest)
# 
# # 5. Evaluate performance
# rmse <- sqrt(mean((pred_test - test$`Claim PD Paid GIC excl. BB`)^2))
# cat("Test RMSE:", rmse, "\n")
# 
# # 6. Convert predictions to time series for plotting
# fitted_ts <- ts(pred_train, start = c(2011, 1), frequency = 12)
# forecast_ts <- ts(pred_test, start = c(2022, 9), frequency = 12)
# 
# # 7. Compute importance and plot
# # Get feature names
# feature_names <- colnames(model_xgb$feature_names)  # or train_matrix if using integer encoding
# 
# # Compute importance
# importance_matrix <- xgb.importance(feature_names = feature_names, model = model_xgb)
# 
# # View top features
# print(head(importance_matrix))
# 
# # Plot importance
# xgb.plot.importance(importance_matrix, top_n = 20)  # adjust top_n as needed
# 
# 
# #
# # 3. Combine SARIMA with XGBoost
# # -----------------------------
# 
# data <- read.csv('data/BNF Demographic Data.csv', check.names = FALSE) %>%
#   select(-`Paid BNF Chapter Code`)
# 
# data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))
# data$`Paid BNF Chapter Description`[data$`Paid BNF Chapter Description` %in% c("", NA)] <- "BLANK CHAPTER" # assign NAs to any blank values
# data$`Age Band (patient age at paid date)`[data$`Age Band (patient age at paid date)` %in% c("", NA)] <- "BLANK AGE BAND" # assign NAs to any blank values
# 
# bnf_demographic <- data %>%
#   mutate(`Paid Date` = dmy(`Paid Date`)) %>%
#   filter(`Disp Health Board Name` == 'NHS AYRSHIRE & ARRAN',
#          # before Sep 24 included so can test against SARIMA
#          `Paid Date` > '2009-12-31' & `Paid Date` < '2024-09-01') %>% # filter data for 2010 onwards so those values can be used as lags for 2011, where there was introduction of free prescriptions
#   group_by(`Disp Health Board Name`, `Paid Date`) %>%
#   summarise(`Claim PD Number of Paid Items` = sum(`Claim PD Number of Paid Items`),
#             `Claim PD Paid GIC excl. BB` = sum(`Claim PD Paid GIC excl. BB`)) %>%
#   ungroup() %>%
#   mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
#   #select(-`Claim PD Paid GIC excl. BB`) %>%
#   # create lagged values
#   #group_by(`Disp Health Board Name`) %>%
#   arrange(`Paid Date`) %>%
#   mutate(lag_1 = lag(`Claim PD Paid GIC excl. BB`, 1),
#          lag_12 = lag(`Claim PD Paid GIC excl. BB`, 12),
#          month = lubridate::month(`Paid Date`),   # seasonal feature
#          year = lubridate::year(`Paid Date`)      # trend feature
#   ) %>%
#   #ungroup() %>%
#   filter(!is.na(lag_12)) # take out 2011 data, imputing 0 would 
# 
# bnf_demographic$`Cost per item`[bnf_demographic$`Cost per item` %in% c("", NA, NaN)] <- 0 # assign NAs to any blank values
# 
# bnf_demographic <- bnf_demographic %>%
#   dplyr::rename("CPI" = `Cost per item`,
#                 "PaidItems" = `Claim PD Number of Paid Items`)
# 
# # 3. Split dataset into train and test data
# train <- bnf_demographic %>%
#   filter(`Paid Date` < max(`Paid Date`) - months(36) + days(1))
# 
# test <- bnf_demographic[-(1:nrow(train)),]
# 
# ## 3.1. Manipulate train and test into matrices
# train_matrix <- train %>% select(-`Disp Health Board Name`, -`Paid Date`, -`Claim PD Paid GIC excl. BB`)
# test_matrix <- test %>% select(-`Disp Health Board Name`, -`Paid Date`, -`Claim PD Paid GIC excl. BB`)
# 
# # One-hot encode everything automatically
# train_matrix_numeric <- model.matrix(~ . - 1, data = train_matrix)
# test_matrix_numeric <- model.matrix(~ . - 1, data = test_matrix)
# 
# # Label must come from corresponding data
# train_targets <- train$`Claim PD Paid GIC excl. BB`
# test_targets <- test$`Claim PD Paid GIC excl. BB`
# 
# dtrain <- xgb.DMatrix(data = train_matrix_numeric, label = train_targets)
# dtest <- xgb.DMatrix(data = test_matrix_numeric, label = test_targets)
# 
# # 3.3. Define parameters with categorical enabled
# params <- list(
#   objective = "reg:squarederror",
#   eta = 0.05,
#   max_depth = 6,
#   subsample = 0.8,
#   colsample_bytree = 0.8
# )
# 
# # 3.4. Train model with early stopping
# model_xgb <- xgb.train(
#   params = params,
#   data = dtrain,
#   nrounds = 10000,
#   watchlist = list(train = dtrain, eval = dtest),
#   early_stopping_rounds = 50,
#   verbose = 1
# )
# 
# 
# #saveRDS(model_xgb, 'model_xgb.rds')
# saveRDS(model_xgb, 'model_xgb_2024.rds') # for comparison with sarima
# model_xgb <- readRDS('model_xgb_2024.rds')
# 
# eval_log <- model_xgb$evaluation_log
# ggplot(eval_log, aes(x = iter)) +
#   geom_line(aes(y = train_rmse, colour = "Train")) +
#   geom_line(aes(y = eval_rmse, colour = "Eval")) +
#   labs(y = "RMSE", x = "Iteration", title = "Training vs Validation RMSE")
# 
# 
# 
# # 4. Predictions
# pred_train <- predict(model_xgb, dtrain)
# pred_test <- predict(model_xgb, dtest)
# 
# # 5. Evaluate performance
# rmse <- sqrt(mean((pred_test - test$`Claim PD Paid GIC excl. BB`)^2))
# cat("Test RMSE:", rmse, "\n")
# 
# # 6. Convert predictions to time series for plotting
# fitted_ts <- ts(pred_train, start = c(2011, 1), frequency = 12)
# forecast_ts <- ts(pred_test, start = c(2021, 9), frequency = 12)
# 
# # 7. Compute importance and plot
# # Get feature names
# feature_names <- colnames(model_xgb$feature_names)  # or train_matrix if using integer encoding
# 
# # Compute importance
# importance_matrix <- xgb.importance(feature_names = feature_names, model = model_xgb)
# 
# # View top features
# print(head(importance_matrix))
# 
# # Plot importance
# xgb.plot.importance(importance_matrix, top_n = 20)  # adjust top_n as needed
# 
# sarima_model <- readRDS('/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast/shiny/forecasts/data/sarima_future_forecast_gic.rds')
# 
# # SARIMA horizon
# sarima_dates <- tail(sarima_model$`Paid Date`, n = 60)
# 
# model <- sarima_model %>%
#   filter(`Paid Date` %in% sarima_dates)
# 
# 
# # Create base future matrix
# future_matrix <- data.frame(
#   month = lubridate::month(sarima_dates),
#   year = lubridate::year(sarima_dates),
#   #sarima_items = model$`Claim PD Number of Paid Items`,       # SARIMA predicted items
#   #sarima_cost = model$`Cost per item`          # SARIMA predicted cost per item
#   sarima_gic = model$`Claim PD Paid GIC excl. BB`
# )
# 
# # Add lag features for GIC
# last_known_gic <- tail(bnf_demographic$`Claim PD Paid GIC excl. BB`, 12)
# future_matrix$lag_1 <- NA
# future_matrix$lag_12 <- NA
# 
# future_matrix <- future_matrix %>%
#   select("GIC" = sarima_gic,
#     #"PaidItems" = sarima_items,
#     #"CPI" = sarima_cost,
#      lag_1, lag_12, month, year)
# 
# # --- Capture training column names and factor levels ---
# train_cols <- colnames(train_matrix_numeric)
# month_levels <- levels(factor(train$month))
# year_levels <- levels(factor(c("2024","2025","2026", "2027", "2028", "2029", "2030")))
# 
# #year_levels <- arrange(factor(levels = train$year, "2026", "2027", "2028", "2029", "2030"))
# 
# 
# # --- Initialise prediction vector ---
# xgb_future_preds <- numeric(nrow(future_matrix))
# 
# # 1. Apply factor levels to future_matrix
# future_matrix$month <- factor(future_matrix$month, levels = month_levels)
# future_matrix$year <- factor(future_matrix$year, levels = year_levels)
# 
# # 2. Fill lag columns recursively
# for (i in 1:nrow(future_matrix)) {
#   future_matrix$lag_1[i] <- ifelse(i == 1, last_known_gic[12], xgb_future_preds[i-1])
#   future_matrix$lag_12[i] <- ifelse(i <= 12, last_known_gic[i], xgb_future_preds[i-12])
# }
# 
# # 3. Encode entire future matrix
# future_matrix_numeric <- model.matrix(~ . - 1, data = future_matrix)
# 
# # 4. Align columns with training
# aligned_future <- matrix(0, nrow = nrow(future_matrix), ncol = length(train_cols))
# colnames(aligned_future) <- train_cols
# common_cols <- intersect(train_cols, colnames(future_matrix_numeric))
# aligned_future[, common_cols] <- future_matrix_numeric[, common_cols]
# 
# # 5. Predict all at once
# dfuture <- xgb.DMatrix(data = aligned_future)
# xgb_future_preds <- predict(model_xgb, newdata = dfuture)
# 
# # --- Combine results ---
# 
# 
# #saveRDS(final_forecast, 'xgboost_forecast.rds')
# 
# sarima_fc <- readRDS('rmse_sarima_gic.rds') %>%
#   filter(type == 'Claim PD Paid GIC excl. BB')
# 
# final_forecast <- data.frame(
#   Paid_Date = sarima_dates,
#   #sarima_items = future_matrix$PaidItems,
#   #sarima_cost = future_matrix$CPI,
#   sarima_gic = sarima_fc$forecast,
#   xgb_gic = xgb_future_preds
# ) %>%
#   #mutate(sarima_gic = (sarima_items * sarima_cost)) %>%
#   mutate(xgb_sarima = (sarima_gic + xgb_gic) / 2)
# 
# saveRDS(final_forecast, 'xgboost_forecast_2024_gic.rds')
# 
# 
# xgboost_fc <- readRDS('xgboost_forecast_2024_gic.rds')
# 
# sarima_rmse <- unique(sarima_fc$rmse)
# 
# w_sarima <- 1/sarima_rmse
# w_xgb <- 1/rmse
# 
# w_sum <- w_sarima + w_xgb
# w_sarima <- w_sarima / w_sum
# w_xgb <- w_xgb / w_sum
# 
# xgboost_fc <- xgboost_fc %>%
#   mutate(ensemble = (w_sarima * sarima_gic) + (w_xgb * xgb_gic))
# 
