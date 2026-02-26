### Prophet Forecast
### Honours Project
### Date last updated: 24/02/2026
### Author: Liam Rooney
### Model: v1.0

### Changes made to previous model version (v...):
### 1. 
### 
###    

### Key notes for running this script:
### Once the variables at lines 48, 49, 50, 56, 70, 72 and 74 have been defined by the user: 
### 1. Click Source --> Source as Workbench Job --> define the memory parameters
### 2. Sit back and let the forecast work its magic! 

options(scipen = 999)

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

# 3. Prophet ----
library(prophet)

## 3.1. Data preparation for Prophet ----
items <- board_data %>%
  filter(`Disp Health Board Name` == 'NHS AYRSHIRE & ARRAN') %>%
  select(ds = `Paid Date`, y = `Claim PD Number of Paid Items`) %>%
  # Change dates to first of month - glitches with February when creating future df on line 100
  mutate(ds = floor_date(ds, 'month'))

ts <- ts(items$y, start = c(2010, 1), frequency = 12)
nsdiffs(ts)

## 3.2. Initial model ----
m <- prophet(items)

m1 <- prophet(items, yearly.seasonality = TRUE)

### 3.2.1. Create future dates ----
future <- make_future_dataframe(m, periods = 12, freq = 'month')
future1 <- make_future_dataframe(m1, periods = 12, freq = 'month')

# Change dates to last of month again ----
future$ds <- ceiling_date(ymd(future$ds), 'month') - days(1)
future1$ds <- ceiling_date(ymd(future1$ds), 'month') - days(1)

## 3.3. Predict using initial model ----
forecast <- predict(m, future)
forecast1 <- predict(m1, future1)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

## 3.3.1. Plot model ----
plot(m, forecast)

### 3.3.2. Plot seasonal components ----
prophet_plot_components(m, forecast)
prophet_plot_components(m1, forecast1)





















