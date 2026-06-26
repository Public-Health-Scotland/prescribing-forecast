# ============================================================
# SARIMA + Tree-Based Hybrid Forecast Script
# Supports:
# - SARIMA + LightGBM
# - SARIMA + XGBoost
# ============================================================

# -------------------------
# 1. Load required packages
# -------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(lightgbm)
library(xgboost)
library(tibble)
library(glue)

## 1.2. Initialise variables and file paths ----
'%!in%' <- function(x, y) !('%in%'(x, y))

path <- '/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast'
setwd(path)

# -------------------------------------------------------
# 2. User inputs
# -------------------------------------------------------
historical_data <- 12
years_to_run <- c(2025)
columns_to_forecast <- c("Claim PD Number of Paid Items", "Claim PD Paid GIC excl. BB")
run_number <- 999

# Choose residual model: "lightgbm" or "xgboost"
residual_model_type <- "lightgbm"

healthboards <- c(
  "NHS AYRSHIRE & ARRAN", "NHS BORDERS", "NHS DUMFRIES & GALLOWAY", "NHS FIFE",
  "NHS FORTH VALLEY", "NHS GRAMPIAN", "NHS GREATER GLASGOW & CLYDE", "NHS HIGHLAND",
  "NHS LANARKSHIRE", "NHS LOTHIAN", "NHS TAYSIDE", "NHS WESTERN ISLES", "NHS ORKNEY",
  "NHS SHETLAND", "SCOTLAND"
)

# -------------------------------------------------------
# 3. Read in data and process
# -------------------------------------------------------
data <- read.csv(
  glue("shiny/forecasts/data/Historical Data.csv"),
  check.names = FALSE
) %>%
  filter(`Presc Health Board Name` %in% healthboards)

data$`Claim PD Paid GIC excl. BB` <- as.double(gsub(",", "", data$`Claim PD Paid GIC excl. BB`))

board_data <- data %>%
  mutate(`Paid Date` = dmy(`Paid Date`)) %>%
  filter(`Paid Date` > as.Date("2009-12-31") & `Paid Date` < as.Date("2025-04-01"))

rm(data)

## 3.1 Aggregate and append Scotland data ----
scotland_data <- board_data %>%
  group_by(`Paid Date`) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(`Presc Health Board Name` = "SCOTLAND") %>%
  select(`Presc Health Board Name`, everything())

board_data <- board_data %>%
  bind_rows(scotland_data) %>%
  mutate(`Cost per item` = `Claim PD Paid GIC excl. BB` / `Claim PD Number of Paid Items`) %>%
  arrange(`Presc Health Board Name`, `Paid Date`)

board_data$`Cost per item`[is.na(board_data$`Cost per item`) | is.nan(board_data$`Cost per item`)] <- 0

healthboards <- unique(board_data$`Presc Health Board Name`)
dates <- unique(board_data$`Paid Date`)
latest_date <- max(board_data$`Paid Date`)

# -------------------------------------------------------
# 4. Load saved SARIMA performance table
# -------------------------------------------------------
latest_forecast_date <- format(Sys.Date(), "%Y-%m-%d")

forecast_performance <- readRDS(
  glue(
    "{path}/shiny/forecasts/sarima/output/",
    historical_data,
    " months/run ",
    run_number,
    "/forecast-performance-2026-06-12.rds"
  )
)

# -------------------------------------------------------
# 5. Helper functions
# -------------------------------------------------------

# Create sequence of month-end dates
month_end_seq <- function(last_date, h) {
  future_month_starts <- floor_date(last_date, unit = "month") %m+% months(1:h)
  ceiling_date(future_month_starts, unit = "month") - days(1)
}

# Build residual feature table
build_residual_features <- function(dates, residuals, max_lag = 12) {
  
  df <- tibble(
    Paid_Date = dates,
    residual = as.numeric(residuals)
  ) %>%
    arrange(Paid_Date)
  
  for (lag in 1:max_lag) {
    df[[paste0("lag", lag)]] <- dplyr::lag(df$residual, lag)
  }
  
  df <- df %>%
    mutate(
      month = month(Paid_Date),
      quarter = quarter(Paid_Date),
      year_num = year(Paid_Date),
      trend = row_number()
    ) %>%
    drop_na()
  
  return(df)
}

# Train residual model
train_residual_model <- function(model_type,
                                 x_train,
                                 y_train,
                                 lgb_params = list(
                                   objective = "regression",
                                   metric = "l2",
                                   learning_rate = 0.05,
                                   num_leaves = 31,
                                   max_depth = -1,           # match Python
                                   min_data_in_leaf = 20,
                                   feature_fraction = 0.8,   # colsample_bytree
                                   bagging_fraction = 0.8,   # subsample
                                   bagging_freq = 1,
                                   lambda_l1 = 0,            # no alpha in your Python LGBM
                                   verbosity = -1
                                 ),
                                 xgb_params = list(
                                   objective = "reg:squarederror",
                                   eta = 0.05,               # learning_rate
                                   max_depth = 6,
                                   subsample = 0.8,
                                   colsample_bytree = 0.8,
                                   min_child_weight = 1,
                                   alpha = 0.5               # match Python
                                 ),
                                 nrounds = ifelse(model_type == "lightgbm", 500, 800)) {
  
  if (model_type == "lightgbm") {
    
    train_data <- lgb.Dataset(data = x_train, label = y_train)
    
    model <- lgb.train(
      params = lgb_params,
      data = train_data,
      nrounds = nrounds
    )
    
  } else if (model_type == "xgboost") {
    
    train_data <- xgb.DMatrix(data = x_train, label = y_train)
    
    model <- xgb.train(
      params = xgb_params,
      data = train_data,
      nrounds = nrounds,
      verbose = 0
    )
    
  } else {
    stop("model_type must be either 'lightgbm' or 'xgboost'")
  }
  
  return(model)
}

# Predict residual model
predict_residual_model <- function(model, model_type, newdata_matrix) {
  
  if (model_type %in% c("lightgbm", "xgboost")) {
    pred <- predict(model, newdata = newdata_matrix)
  } else {
    stop("model_type must be either 'lightgbm' or 'xgboost'")
  }
  
  return(as.numeric(pred))
}

# Recursive forecast of residuals
forecast_residuals_recursive <- function(model,
                                         model_type,
                                         residual_history,
                                         future_dates,
                                         max_lag = 12,
                                         start_trend) {
  
  residual_history <- as.numeric(residual_history)
  preds <- numeric(length(future_dates))
  
  for (i in seq_along(future_dates)) {
    
    current_date <- future_dates[i]
    
    feature_row <- tibble(
      month = month(current_date),
      quarter = quarter(current_date),
      year_num = year(current_date),
      trend = start_trend + i
    )
    
    for (lag in 1:max_lag) {
      feature_row[[paste0("lag", lag)]] <- residual_history[length(residual_history) - lag + 1]
    }
    
    pred <- predict_residual_model(
      model = model,
      model_type = model_type,
      newdata_matrix = as.matrix(feature_row)
    )
    
    preds[i] <- pred
    residual_history <- c(residual_history, pred)
  }
  
  return(preds)
}

# Fit hybrid model and forecast future periods
forecast_hybrid_future <- function(df,
                                   value_col,
                                   model_type = residual_model_type,
                                   h = 36,
                                   p,
                                   q,
                                   P,
                                   Q,
                                   d = 1,
                                   D = 1,
                                   max_lag = 12,
                                   nrounds = 250) {
  
  df <- df %>%
    arrange(`Paid Date`) %>%
    select(`Paid Date`, all_of(value_col))
  
  y <- df[[value_col]]
  dates <- df$`Paid Date`
  
  # Build time series
  y_ts <- ts(
    y,
    frequency = 12,
    start = c(year(min(dates)), month(min(dates)))
  )
  
  # -----------------------------
  # 1. Fit SARIMA
  # -----------------------------
  sarima_fit <- Arima(
    y_ts,
    order = c(p, d, q),
    seasonal = list(order = c(P, D, Q), period = 12)
  )
  
  sarima_fc <- forecast(sarima_fit, h = h)
  sarima_pred <- as.numeric(sarima_fc$mean)
  
  # -----------------------------
  # 2. Extract residuals
  # -----------------------------
  fitted_vals <- as.numeric(fitted(sarima_fit))
  residuals_all <- as.numeric(y - fitted_vals)
  
  # -----------------------------
  # 3. Build residual features
  # -----------------------------
  residual_df <- build_residual_features(
    dates = dates,
    residuals = residuals_all,
    max_lag = max_lag
  )
  
  x_train <- residual_df %>%
    select(-Paid_Date, -residual) %>%
    as.matrix()
  
  y_train <- residual_df$residual
  
  # -----------------------------
  # 4. Train chosen tree model on residuals
  # -----------------------------
  residual_model <- train_residual_model(
    model_type = model_type,
    x_train = x_train,
    y_train = y_train,
    nrounds = nrounds
  )
  
  # -----------------------------
  # 5. Forecast residuals
  # -----------------------------
  future_dates <- month_end_seq(max(dates), h)
  
  residual_pred <- forecast_residuals_recursive(
    model = residual_model,
    model_type = model_type,
    residual_history = residuals_all,
    future_dates = future_dates,
    max_lag = max_lag,
    start_trend = nrow(residual_df)
  )
  
  # -----------------------------
  # 6. Hybrid forecast
  # -----------------------------
  hybrid_forecast <- sarima_pred + residual_pred
  
  # ------------------------------------------------
  # 7. Approximate hybrid intervals using SARIMA CI
  # ------------------------------------------------
  lower_80 <- as.numeric(sarima_fc$lower[, 1] + residual_pred)
  upper_80 <- as.numeric(sarima_fc$upper[, 1] + residual_pred)
  lower_95 <- as.numeric(sarima_fc$lower[, 2] + residual_pred)
  upper_95 <- as.numeric(sarima_fc$upper[, 2] + residual_pred)
  
  out <- tibble(
    Date = future_dates,
    SARIMA = sarima_pred,
    Residual_Pred = residual_pred,
    Hybrid = hybrid_forecast,
    Lower_80 = lower_80,
    Upper_80 = upper_80,
    Lower_95 = lower_95,
    Upper_95 = upper_95,
    Hybrid_Model = case_when(
      model_type == "lightgbm" ~ "SARIMA + LightGBM",
      model_type == "xgboost" ~ "SARIMA + XGBoost"
    )
  )
  
  return(list(
    forecast = out,
    sarima_model = sarima_fit,
    residual_model = residual_model,
    residual_history = residuals_all
  ))
}

# -------------------------------------------------------
# 6. Main loop: create hybrid forecast table
# -------------------------------------------------------
num_of_months <- historical_data
forecast_horizon <- num_of_months + 36

combined_forecast <- tibble()

for (hb in healthboards) {
  
  for (column in columns_to_forecast) {
    
    for (y in years_to_run) {
      
      message(glue("Running {residual_model_type} hybrid forecast for {hb} | {column} | {y}"))
      
      # -----------------------------
      # A. Recreate the same data window
      # -----------------------------
      diff <- year(latest_date) - y
      
      date_adj <- board_data %>%
        filter(`Paid Date` == max(`Paid Date`) - months(12 * diff))
      
      window_max <- unique(date_adj$`Paid Date`)
      
      if (length(window_max) == 0) {
        message(glue("No matching window_max found for year {y}"))
        next
      }
      
      window_max <- max(window_max)
      MaxDate <- window_max - months(num_of_months)
      
      df <- board_data %>%
        filter(`Presc Health Board Name` == hb) %>%
        select(`Paid Date`, all_of(column)) %>%
        filter(`Paid Date` > as.Date("2011-03-31") & `Paid Date` < MaxDate + days(1)) %>%
        arrange(`Paid Date`)
      
      # Skip if not enough data
      if (nrow(df) <= 24) {
        message(glue("Skipping {hb} | {column} | {y} - insufficient data"))
        next
      }
      
      # -----------------------------
      # B. Pull best SARIMA parameters
      # -----------------------------
      filtered_list <- forecast_performance %>%
        filter(
          board == hb,
          type == column,
          year == y
        ) %>%
        slice(1)
      
      if (nrow(filtered_list) == 0) {
        message(glue("No saved SARIMA parameters found for {hb} | {column} | {y}"))
        next
      }
      
      p <- filtered_list$p[[1]]
      q <- filtered_list$q[[1]]
      P <- filtered_list$P[[1]]
      Q <- filtered_list$Q[[1]]
      
      # -----------------------------
      # C. Run hybrid model
      # -----------------------------
      result <- tryCatch({
        
        hybrid_future <- forecast_hybrid_future(
          df = df,
          value_col = column,
          model_type = residual_model_type,
          h = forecast_horizon,
          p = p,
          q = q,
          P = P,
          Q = Q,
          d = 1,
          D = 1,
          max_lag = 12,
          nrounds = 250
        )
        
        hybrid_future$forecast %>%
          mutate(
            Board = hb,
            Type = column,
            Year = y,
            Historical_Data = paste0(num_of_months, " months of historical data"),
            F_Horizon = forecast_horizon,
            p = p,
            q = q,
            P = P,
            Q = Q,
            Hybrid_Error = FALSE
          ) %>%
          select(
            Board,
            Date,
            SARIMA,
            Residual_Pred,
            Hybrid,
            Lower_80,
            Upper_80,
            Lower_95,
            Upper_95,
            Type,
            Year,
            Historical_Data,
            F_Horizon,
            p,
            q,
            P,
            Q,
            Hybrid_Model,
            Hybrid_Error
          )
        
      }, error = function(e) {
        
        message(glue("Hybrid model error for {hb} | {column} | {y}: {e$message}"))
        
        tibble(
          Board = hb,
          Date = seq.Date(Sys.Date(), by = "month", length.out = forecast_horizon),
          SARIMA = NA_real_,
          Residual_Pred = NA_real_,
          Hybrid = NA_real_,
          Lower_80 = NA_real_,
          Upper_80 = NA_real_,
          Lower_95 = NA_real_,
          Upper_95 = NA_real_,
          Type = column,
          Year = y,
          Historical_Data = paste0(num_of_months, " months of historical data"),
          F_Horizon = forecast_horizon,
          p = p,
          q = q,
          P = P,
          Q = Q,
          Hybrid_Model = case_when(
            residual_model_type == "lightgbm" ~ "SARIMA + LightGBM",
            residual_model_type == "xgboost" ~ "SARIMA + XGBoost"
          ),
          Hybrid_Error = TRUE
        )
      })
      
      combined_forecast <- bind_rows(combined_forecast, result)
    }
  }
}

# -------------------------------------------------------
# 7. Save hybrid forecast output
# -------------------------------------------------------
write.xlsx(
  combined_forecast,
  glue(
    "{path}/shiny/forecasts/hybrid/hybrid-forecast-",
    residual_model_type,
    "-",
    latest_forecast_date,
    ".xlsx"
  )
)

message(glue("{unique(combined_forecast$Hybrid_Model)} forecasting complete."))

xgboost <- read_excel("shiny/forecasts/hybrid/hybrid-forecast-xgboost-2026-06-21.xlsx") %>%
  filter(!Board == "SCOTLAND") %>%
  mutate(Date = ymd(Date)) %>%
  group_by(Date, Type) %>%
  summarise(SARIMA = sum(SARIMA),
            SARIMA_XGBoost = sum(Hybrid)) %>%
  ungroup() %>%
  mutate(Board = "SCOTLAND") %>%
  select(Board, Date, Type, SARIMA, SARIMA_XGBoost)

write.xlsx(xgboost, "shiny/forecasts/hybrid/sarima_xgboost.xlsx")

lightgbm <- read_excel("shiny/forecasts/hybrid/hybrid-forecast-lightgbm-2026-06-21.xlsx") %>%
  filter(!Board == "SCOTLAND") %>%
  mutate(Date = ymd(Date)) %>%
  group_by(Date, Type) %>%
  summarise(SARIMA = sum(SARIMA),
            SARIMA_LightGBM = sum(Hybrid)) %>%
  ungroup() %>%
  mutate(Board = "SCOTLAND") %>%
  select(Board, Date, Type, SARIMA, SARIMA_LightGBM)

write.xlsx(lightgbm, "shiny/forecasts/hybrid/sarima_lightgbm.xlsx")

sarima_xgb_lgb <- left_join(xgboost, lightgbm)

historic <- readRDS(paste0('shiny/forecasts/sarima/output/forecast-run-4.rds')) %>%
  filter(Board == "SCOTLAND",
         Date > "2024-03-31",
         Historical_Data == "12 months of historical data",
         #Year == 2025,
         F_Horizon == 48,
         Arima_Error == FALSE) %>%
  select(Board, Date, Type, Observed = Measure)

df <- left_join(historic, sarima_xgb_lgb)

write.xlsx(df, "shiny/forecasts/hybrid/sarima_xgb_lgb.xlsx")

accuracy_mape <- df %>%
  mutate(FY = extract_fin_year(Date)) %>%
  select(Board, FY, Date, everything()) %>%
  group_by(Board, FY, Type) %>%
  summarise(Observed = sum(Observed),
            SARIMA = sum(SARIMA),
            SARIMA_XGBoost = sum(SARIMA_XGBoost),
            SARIMA_LightGBM = sum(SARIMA_LightGBM)) %>%
  filter(!Type == "Cost per item") %>%
  pivot_longer(5:7,
               names_to = "Model",
               values_to = "Prediction") %>%
  select(Board, FY, Type, Model, everything()) %>%
  mutate(MAPE = (abs(Observed - Prediction) / Observed) * 100)
